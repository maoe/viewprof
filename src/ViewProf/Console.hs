{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module ViewProf.Console where
import Control.Arrow ((&&&))
import Data.Foldable
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Text.Printf
import qualified Data.List.NonEmpty as NE

import Brick hiding (on)
import Control.Lens hiding (views)
import Data.Set (Set)
import Graphics.Vty
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as Merge
import qualified GHC.Prof as Prof

data Profile = Profile
  { _report :: Prof.Profile
  , _views :: NonEmpty View
  }

data View
  = AggregatesView
    { _costCentres :: !(V.Vector Prof.AggregateCostCentre)
    , _focus :: !Int
    }
  | CallSitesView
    { _callee :: !Prof.AggregateCostCentre
    , _callSites :: !(V.Vector (Prof.CallSite Prof.AggregateCostCentre))
    , _focus :: !Int
    , _expanded :: !(Set Int)
    }
  | ModulesView
    { _modules :: !(V.Vector Prof.AggregateModule)
    , _focus :: !Int
    }

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

handleProfileEvent :: Profile -> BrickEvent Name e -> EventM Name (Next Profile)
handleProfileEvent prof@Profile {..} ev = case ev of
  VtyEvent vtyEv -> case vtyEv of
    EvKey key []
      | key `elem` [KEsc, KChar 'q'] -> if null (NE.tail (prof ^. views))
        then halt prof
        else do
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
    _ -> case NE.head _views of
      AggregatesView {} -> case vtyEv of
        EvKey (KChar 't') [] -> do
          invalidateCache
          continue $! sortCostCentresBy
            (Prof.aggregateCostCentreTime &&& Prof.aggregateCostCentreAlloc)
            prof
        EvKey (KChar 'a') [] -> do
          invalidateCache
          continue $! sortCostCentresBy
            (Prof.aggregateCostCentreAlloc &&& Prof.aggregateCostCentreTime)
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
        _ -> continue prof
      ModulesView {} -> continue prof
  _ -> continue prof
  where
    popView p = case NE.nonEmpty (NE.tail _views) of
      Nothing -> p
      Just xs -> p & views .~ xs
    moveUp p = p & currentFocus %~ (\i -> max 0 (i - 1))
    moveDown p = p & currentFocus %~ (\i -> min (len - 1) (i + 1))
      where
        len = case p ^. topView of
          AggregatesView {_costCentres} -> V.length _costCentres
          CallSitesView {_callSites} -> V.length _callSites
          ModulesView {_modules} -> V.length _modules
    sortCostCentresBy key p = p & topView . costCentres
      %~ V.modify (Merge.sortBy (flip compare `on` key))
    sortCallSitesBy key p = p & topView . callSites
      %~ V.modify (Merge.sortBy (flip compare `on` key))
    displayCostCentres p = p & views .~ AggregatesView
      { _costCentres = V.fromList $ Prof.aggregateCostCentres $ p ^. report
      , _focus = 0
      } NE.:| []
    displayCallers p = fromMaybe p $ do
      let !model = p ^. topView . costCentres
          !idx = p ^. currentFocus
      Prof.AggregateCostCentre {..} <- model V.!? idx
      (_callee, callers) <- Prof.aggregateCallSites
        aggregateCostCentreName
        aggregateCostCentreModule
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
drawProfile prof = do
  viewState <- NE.toList $ prof ^. views
  return $ case viewState of
    AggregatesView {..} -> viewport AggregatesViewport Vertical $
      vBox $ V.toList $
        flip V.imap _costCentres $ \i row -> cached (AggregatesCache i) $
          let widget = drawAggregateCostCentre row
          in if i == _focus
            then withAttr selectedAttr (visible widget)
            else widget
    CallSitesView {..} -> viewport CallSitesViewport Vertical $
      vBox
        [ drawAggregateCostCentre _callee
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

drawAggregateCostCentre :: Prof.AggregateCostCentre -> Widget n
drawAggregateCostCentre Prof.AggregateCostCentre {..} = hBox
  [ txt aggregateCostCentreModule
  , txt "."
  , padRight Max $ txt aggregateCostCentreName
  , padRight (Pad 1) $ str (formatPercentage aggregateCostCentreTime)
  , str (formatPercentage aggregateCostCentreAlloc)
  ]

drawCallSite
  :: Prof.AggregateCostCentre
  -> Prof.CallSite Prof.AggregateCostCentre
  -> Widget n
drawCallSite Prof.AggregateCostCentre {..} Prof.CallSite {..} = hBox
  [ txt $ Prof.aggregateCostCentreModule callSiteCostCentre
  , txt "."
  , padRight Max $ txt $ Prof.aggregateCostCentreName callSiteCostCentre
  , padRight (Pad 1) $ hBox
    [ str $ contribution callSiteContribTime aggregateCostCentreTime
    , txt " ("
    , str $ formatPercentage callSiteContribTime
    , txt ")"
    ]
  , hBox
    [ str $ contribution callSiteContribAlloc aggregateCostCentreAlloc
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
