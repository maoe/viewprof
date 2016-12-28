{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
import Data.List.NonEmpty
import System.Environment

import Brick
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified GHC.Prof as Prof
import qualified Graphics.Vty as Vty

import ViewProf.Console

main :: IO ()
main = do
  path:_ <- getArgs
  profile <- parseProfile path
  void $ defaultMain app profile

parseProfile :: FilePath -> IO (Profile Name)
parseProfile path = do
  text <- TL.readFile path
  case Prof.decode text of
    Left reason -> fail reason
    Right prof -> return Profile
      { _profileReport = prof
      , _profileViewStates = AggregatesView
        { _viewModel = V.fromList (Prof.aggregateCostCentres prof)
        , _viewFocus = 0
        } :| []
      , _profileName = undefined
      }

app :: App (Profile Name) e Name
app = App
  { appDraw = drawProfile
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleProfileEvent
  , appStartEvent = return
  , appAttrMap = const $ attrMap Vty.defAttr
    [ (selectedAttr, Vty.black `on` Vty.white)
    ]
  }
