{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Main (
    main
  ) where

import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)

import Reflex
import Reflex.Brick

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

data MyState =
  MyState {
    _count :: Int
  }

myStateToAppState :: MyState -> ReflexBrickAppState n
myStateToAppState (MyState c) =
  let
    w = withBorderStyle BS.unicodeBold .
        B.borderWithLabel (str "Count") .
        C.hCenter .
        padAll 1 .
        str .
        show $
        c
    ws = [w]
  in
    ReflexBrickAppState ws (const Nothing) (attrMap V.defAttr [])

main :: IO ()
main =
  runReflexBrickApp @() (pure ()) $ \es -> do
    (eTick, runTick) <- newTriggerEvent

    let eQuit = select es $ RBVtyEvent (RBKey $ V.KChar 'q')

    dState <-
      foldDyn ($) (MyState 0) $
      mergeWith (.) [ MyState . succ . _count <$ eTick ]

    _ <-
      liftIO . forkIO . forever $ do
        threadDelay 1000000
        runTick ()

    pure $ ReflexBrickApp (myStateToAppState <$> dState) never (void eQuit)
