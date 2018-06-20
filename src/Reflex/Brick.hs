{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Brick (
    ReflexBrickAppState(..)
  , runReflexBrickApp
  ) where

import Control.Monad (void, forever)

import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Reflex
import Reflex.Host.Basic

import Brick
import Brick.Main
import Brick.Types
import qualified Graphics.Vty as V

data RBNext s =
    RBContinue s
  | RBHalt s
  | RBSuspendAndResume (IO s)

data ReflexBrickAppState n =
  ReflexBrickAppState {
    rbasWidgets :: [Widget n]
  , rbasCursorFn :: [CursorLocation n] -> Maybe (CursorLocation n)
  , rbasAttrMap :: AttrMap
  }

data ReflexBrickEvents e n =
  ReflexBrickEvents {
    rbeToReflex :: TMVar (BrickEvent n e)
  , rbeFromReflex :: TMVar (RBNext (ReflexBrickAppState n))
  }

mkReflexApp :: EventM n () -> IO (ReflexBrickEvents () n, App (ReflexBrickAppState n) () n)
mkReflexApp initial = do
  (toReflex, fromReflex) <- atomically $ (,) <$> newEmptyTMVar <*> newEmptyTMVar
  let
    process s e = do
      x <- liftIO $ do
        atomically $ putTMVar toReflex e
        atomically $ takeTMVar fromReflex
      case x of
        RBContinue s -> continue s
        RBHalt s -> halt s
        RBSuspendAndResume s -> suspendAndResume s
    events = ReflexBrickEvents toReflex fromReflex
    app = App rbasWidgets rbasCursorFn process (\s -> s <$ initial) rbasAttrMap
  pure $ (events, app)

runReflexBrickApp :: Ord n
                  => EventM n ()
                  -> ReflexBrickAppState n
                  -> (forall t m. BasicGuestConstraints t m => ReflexBrickAppState n -> Event t (BrickEvent n ()) -> BasicGuest t m (Event t (RBNext (ReflexBrickAppState n))))
                  -> IO ()
runReflexBrickApp initial initialState fn = do
  (events, app) <- mkReflexApp initial
  basicHostWithQuit $ do
    (eQuit, onQuit) <- newTriggerEvent
    (eEventIn, onEventIn) <- newTriggerEvent

    liftIO . forkIO $ do
      void $ defaultMain app initialState
      onQuit ()

    liftIO . forkIO . forever $ do
      e <- atomically $ takeTMVar (rbeToReflex events)
      onEventIn e

    eEventOut <- fn initialState eEventIn
    performEvent_ $ liftIO . atomically . putTMVar (rbeFromReflex events) <$> eEventOut

    pure ((), eQuit)

-- testMe :: IO ()
-- testMe =
--   let
--     initialState :: ReflexBrickAppState ()
--     initialState =
--       let
--         ws = [str "Hi"]
--       in
--         ReflexBrickAppState ws (const Nothing) (attrMap V.defAttr [])
--   in
--     runReflexBrickApp (pure ()) initialState $ \i e -> do
--       let
--         isQuit e = e == VtyEvent (V.EvKey (V.KChar 'q') [])
--         eQuit = ffilter isQuit e
--         eNotQuit = difference e eQuit
--         eOut = leftmost [RBContinue i <$ eNotQuit, RBHalt i <$ eQuit]
--       pure eOut
