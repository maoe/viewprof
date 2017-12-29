{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Arrow ((&&&))
import Control.Monad
import Data.Foldable
import Data.Function (on)
import Data.List.NonEmpty hiding (head, length)
import Data.Maybe
import System.Environment
import System.Exit (exitFailure)
import Text.Printf
import qualified Data.List.NonEmpty as NE

import Brick hiding (on)
import Control.Lens hiding (views)
import Data.Set (Set)
import Graphics.Vty
import qualified Brick
import qualified Brick.Widgets.Border as Brick
import qualified Brick.Widgets.Center as Brick
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified GHC.Prof as Prof

data Profile = Profile
  { _report :: Prof.Profile
  , _views :: NonEmpty View
  , _modalView :: Maybe ModalView
  , _lastKeyEvent :: !(Maybe (Key, [Modifier]))
  }

data View
  = AggregatesView
    { _costCentres :: !(V.Vector Prof.AggregatedCostCentre)
    , _focus :: !Int
    }
  | CallSitesView
    { _callee :: !Prof.AggregatedCostCentre
    , _callSites :: !(V.Vector (Prof.CallSite Prof.AggregatedCostCentre))
    , _focus :: !Int
    , _expanded :: !(Set Int)
    }
  | ModulesView
    { _modules :: !(V.Vector Prof.AggregateModule)
    , _focus :: !Int
    }

data ModalView
  = InfoView
  | HelpView

makeLenses ''Profile
makeLenses ''View

data Name
  = AggregatesViewport
  | CallSitesViewport
  | ModulesViewport
  | AggregatesCache !Int
  | CallSitesCache !Int
  | ModulesCache !Int
  deriving (Eq, Ord, Show)

data Args =
  Args { profilePath :: FilePath
       }

usage :: IO a
usage = do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " <file.prof>"
  exitFailure

parseArgs :: IO Args
parseArgs = do
  args <- getArgs
  if length args /= 1
     then usage
     else return $ Args (head args)

main :: IO ()
main = do
  args <- parseArgs
  profile <- parseProfile $ profilePath args
  void $ defaultMain app profile

parseProfile :: FilePath -> IO Profile
parseProfile path = do
  contents <- TL.readFile path
  case Prof.decode contents of
    Left reason -> fail reason
    Right prof -> return Profile
      { _report = prof
      , _views = AggregatesView
        { _costCentres = V.fromList (Prof.aggregatedCostCentres prof)
        , _focus = 0
        } :| []
      , _modalView = Nothing
      , _lastKeyEvent = Nothing
      }

app :: App Profile e Name
app = App
  { appDraw = drawProfile
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleProfileEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap defAttr
    [ (selectedAttr, black `Brick.on` white)
    ]
  }

handleProfileEvent :: Profile -> BrickEvent Name e -> EventM Name (Next Profile)
handleProfileEvent prof@Profile {..} ev = case ev of
  VtyEvent vtyEv -> case vtyEv of
    EvResize {} -> do
      invalidateCache
      continue prof
    EvKey key []
      | key `elem` [KEsc, KChar 'q'] -> if
        | Just _ <- prof ^. modalView -> do
          invalidateCache
          continue $! prof & modalView .~ Nothing
        | null (NE.tail (prof ^. views)) -> halt prof
        | otherwise -> do
          invalidateCache
          continue $! popView prof
      | key `elem` [KUp, KChar 'k'] -> do
        let !pos = prof ^. currentFocus
        for_ [pos, pos-1] $ invalidateCacheEntry . currentCacheEntry prof
        continue $! moveUp prof
      | key `elem` [KDown, KChar 'j'] -> do
        let !pos = prof ^. currentFocus
        for_ [pos, pos+1] $ invalidateCacheEntry . currentCacheEntry prof
        continue $! moveDown prof
      | key `elem` [KChar 'C'] -> do
        invalidateCache
        continue $! displayCostCentres prof
      | key `elem` [KChar 'M'] -> do
        invalidateCache
        continue $! displayModules prof
      | key `elem` [KChar 'g'] ->
        if prof ^. lastKeyEvent == Just (KChar 'g', [])
          then do
            invalidateCache
            continue $! moveToTop $ prof & lastKeyEvent .~ Nothing
          else
            continue $! prof & lastKeyEvent .~ Just (key, [])
      | key `elem` [KChar 'G'] -> do
        invalidateCache
        continue $! moveToEnd prof
      | key `elem` [KChar 'i'] ->
        continue $! prof & modalView ?~ InfoView
      | key `elem` [KChar 'h', KChar '?'] ->
        continue $! prof & modalView ?~ HelpView
    _ -> case NE.head _views of
      AggregatesView {} -> case vtyEv of
        EvKey (KChar 't') [] -> do
          invalidateCache
          continue $! sortCostCentresBy
            (Prof.aggregatedCostCentreTime &&& Prof.aggregatedCostCentreAlloc)
            prof
        EvKey (KChar 'a') [] -> do
          invalidateCache
          continue $! sortCostCentresBy
            (Prof.aggregatedCostCentreAlloc &&& Prof.aggregatedCostCentreTime)
            prof
        EvKey (KChar 'e') [] -> do
          invalidateCache
          continue $! sortCostCentresBy
            Prof.aggregatedCostCentreEntries
            prof
        EvKey key []
          | key `elem` [KEnter] -> do
            invalidateCache
            continue $! displayCallers prof
        _ -> continue prof
      CallSitesView {} -> case vtyEv of
        EvKey (KChar 't') [] -> do
          invalidateCache
          continue $! sortCallSitesBy
            (Prof.callSiteContribTime &&& Prof.callSiteContribAlloc)
            prof
        EvKey (KChar 'a') [] -> do
          invalidateCache
          continue $! sortCallSitesBy
            (Prof.callSiteContribAlloc &&& Prof.callSiteContribTime)
            prof
        EvKey (KChar 'e') [] -> do
          invalidateCache
          continue $! sortCallSitesBy
            Prof.callSiteContribEntries
            prof
        _ -> continue prof
      ModulesView {} -> continue prof
  _ -> continue prof
  where
    popView p = case NE.nonEmpty (NE.tail _views) of
      Nothing -> p
      Just xs -> p & views .~ xs
    moveUp p = p & currentFocus %~ (\i -> max 0 (i - 1))
    moveDown p = p & currentFocus %~ (\i -> min (focusLen p - 1) (i + 1))
    moveToTop p = p & currentFocus .~ 0
    moveToEnd p = p & currentFocus .~ focusLen p - 1
    focusLen p = case p ^. topView of
      AggregatesView {_costCentres} -> V.length _costCentres
      CallSitesView {_callSites} -> V.length _callSites
      ModulesView {_modules} -> V.length _modules
    sortCostCentresBy key p = p & topView . costCentres
      %~ V.modify (Merge.sortBy (flip compare `on` key))
    sortCallSitesBy key p = p & topView . callSites
      %~ V.modify (Merge.sortBy (flip compare `on` key))
    displayCostCentres p = p & views .~ AggregatesView
      { _costCentres = V.fromList $ Prof.aggregatedCostCentres $ p ^. report
      , _focus = 0
      } NE.:| []
    displayCallers p = fromMaybe p $ do
      let !model = p ^. topView . costCentres
          !idx = p ^. currentFocus
      Prof.AggregatedCostCentre {..} <- model V.!? idx
      (_callee, callers) <- Prof.aggregateCallSites
        aggregatedCostCentreName
        aggregatedCostCentreModule
        (p ^. report)
      return $! p & views %~ NE.cons CallSitesView
        { _callee
        , _callSites = V.fromList callers
        , _focus = 0
        , _expanded = Set.empty
        }
    displayModules p = p & views .~ ModulesView
      { _modules = V.fromList $ Prof.aggregateModules $ p ^. report
      , _focus = 0
      } NE.:| []

topView :: Lens' Profile View
topView = views . lens NE.head (\(_ NE.:| xs) x -> x NE.:| xs)
{-# INLINE topView #-}

currentFocus :: Lens' Profile Int
currentFocus = topView . focus
{-# INLINE currentFocus #-}

currentViewport :: Profile -> Name
currentViewport p = case p ^. topView of
  AggregatesView {} -> AggregatesViewport
  CallSitesView {} -> CallSitesViewport
  ModulesView {} -> ModulesViewport

currentCacheEntry :: Profile -> Int -> Name
currentCacheEntry p n = case p ^. topView of
  AggregatesView {} -> AggregatesCache n
  CallSitesView {} -> CallSitesCache n
  ModulesView {} -> ModulesCache n

profileAttr :: AttrName
profileAttr = "profile"

selectedAttr :: AttrName
selectedAttr = "selected"

drawProfile :: Profile -> [Widget Name]
drawProfile prof =
  [ maybe emptyWidget (Brick.centerLayer . drawModalView) (prof ^. modalView)
  , drawView $ NE.head $ prof ^. views
  ]
  where
    drawModalView = \case
      InfoView -> Brick.border $ vBox
        [ txt $ prof ^. report . to Prof.profileCommandLine
        -- TODO: add more
        ]
      HelpView -> Brick.borderWithLabel (txt "Help") $ vBox
        [ txt "Keyboard shortcuts"
        , txt "q       quit current view"
        , txt "j,down  move down"
        , txt "k,up    move up"
        , txt "gg      move to the top"
        , txt "G       move to the bottom"
        , txt "C       switch to aggregate cost centre view"
        , txt "enter   switch to call site view"
        , txt "M       switch to module level breakdown"
        , txt "i       display profiling info"
        , txt "h       display help message"
        , txt "t       sort by time"
        , txt "a       sort by allocation"
        , txt "e       sort by number of entries"
        ]
    drawView = \case
      AggregatesView {..} -> viewport AggregatesViewport Vertical $
        vBox $ V.toList $
          flip V.imap _costCentres $ \i row -> cached (AggregatesCache i) $
            let widget = drawAggregatedCostCentre row
            in if i == _focus
              then withAttr selectedAttr (visible widget)
              else widget
      CallSitesView {..} -> viewport CallSitesViewport Vertical $
        vBox
          [ drawAggregatedCostCentre _callee
          , vBox $ V.toList $ flip V.imap _callSites $ \i row ->
            cached (CallSitesCache i) $
              let widget = drawCallSite _callee row
              in if i == _focus
                then withAttr selectedAttr (visible widget)
                else widget
          ]
      ModulesView {..} -> viewport ModulesViewport Vertical $
        vBox $ V.toList $
          flip V.imap _modules $ \i row -> cached (ModulesCache i) $
            let widget = drawAggregateModule row
            in if i == _focus
              then withAttr selectedAttr (visible widget)
              else widget

drawAggregatedCostCentre :: Prof.AggregatedCostCentre -> Widget n
drawAggregatedCostCentre Prof.AggregatedCostCentre {..} = hBox
  [ txt aggregatedCostCentreModule
  , txt "."
  , padRight Max $ txt aggregatedCostCentreName
  , maybe emptyWidget (padRight (Pad 1) . str . show) aggregatedCostCentreEntries
  , padRight (Pad 1) $ str (formatPercentage aggregatedCostCentreTime)
  , str (formatPercentage aggregatedCostCentreAlloc)
  ]

drawCallSite
  :: Prof.AggregatedCostCentre
  -> Prof.CallSite Prof.AggregatedCostCentre
  -> Widget n
drawCallSite Prof.AggregatedCostCentre {..} Prof.CallSite {..} = hBox
  [ txt $ Prof.aggregatedCostCentreModule callSiteCostCentre
  , txt "."
  , padRight Max $ txt $ Prof.aggregatedCostCentreName callSiteCostCentre
  , padRight (Pad 1) $ str $ show callSiteContribEntries
  , padRight (Pad 1) $ hBox
    [ str $ contribution callSiteContribTime aggregatedCostCentreTime
    , txt " ("
    , str $ formatPercentage callSiteContribTime
    , txt ")"
    ]
  , hBox
    [ str $ contribution callSiteContribAlloc aggregatedCostCentreAlloc
    , txt " ("
    , str $ formatPercentage callSiteContribAlloc
    , txt ")"
    ]
  ]
  where
    contribution part whole
      | whole == 0 = formatPercentage 0
      | otherwise = formatPercentage $ Sci.fromFloatDigits $
        100 * (Sci.toRealFloat part / Sci.toRealFloat whole :: Double)

drawAggregateModule :: Prof.AggregateModule -> Widget n
drawAggregateModule Prof.AggregateModule {..} = hBox
  [ txt aggregateModuleName
  , padLeft Max $ str (formatPercentage aggregateModuleTime)
  , padLeft (Pad 1) $ str (formatPercentage aggregateModuleAlloc)
  ]

formatPercentage :: Sci.Scientific -> String
formatPercentage = printf "%5s%%" . Sci.formatScientific Sci.Fixed (Just 1)
