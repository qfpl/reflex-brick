{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module Main (
    main
  ) where

import Control.Concurrent (threadDelay)

import Control.Lens

import Reflex
import Reflex.Brick

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

data MyState = MyState { _count :: Int }

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
  let
    initial =
      MyState 0
    tick =
      threadDelay 1000000
  in
    runReflexBrickApp (pure ()) (Just tick) (myStateToAppState initial :: ReflexBrickAppState ()) $ \_ es -> mdo
      dState <- foldDyn ($) initial .
                mergeWith (.) $ [
                  MyState . succ . _count <$ eTick
                ]
      let
        eTick =
          select es RBAppEvent
        eQuit =
          select es $ RBKey (V.KChar 'q')

        eNotQuit =
          difference (updated dState) eQuit

        dAppState =
          myStateToAppState <$> dState
        eOut =
          leftmost [
              RBContinue <$> current dAppState <@ eNotQuit
            , RBHalt     <$> current dAppState <@ eQuit
            ]
      pure eOut
