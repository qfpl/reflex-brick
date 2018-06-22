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
    runReflexBrickApp
  , switchReflexBrickApp
  , ReflexBrickApp(..)
  , module Reflex.Brick.Types
  , module Reflex.Brick.Events
  ) where

import Control.Monad (void, forever)
import Data.Maybe (fromMaybe)

import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)

import Reflex
import Reflex.Host.Basic

import Brick.Types (BrickEvent, EventM)
import Brick.Main (App(..), customMain, continue, halt, suspendAndResume)
import Brick.BChan (newBChan, writeBChan)

import qualified Graphics.Vty as V

import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (singleton)

import Reflex.Brick.Types
import Reflex.Brick.Events

data ReflexBrickEvents e n =
  ReflexBrickEvents {
    rbeToReflex   :: MVar (BrickEvent n e)
  , rbeFromReflex :: MVar (RBNext (ReflexBrickAppState n))
  }

mkReflexApp :: EventM n e -> IO (ReflexBrickEvents e n, App (ReflexBrickAppState n) e n)
mkReflexApp initial = do
  (toReflex, fromReflex) <- (,) <$> newEmptyMVar <*> newEmptyMVar
  let
    process _ e = do
      x <- liftIO $ do
        putMVar toReflex e
        takeMVar fromReflex
      case x of
        RBContinue s -> continue s
        RBHalt s -> halt s
        RBSuspendAndResume s -> suspendAndResume s
    events = ReflexBrickEvents toReflex fromReflex
    app = App _rbWidgets _rbCursorFn process (<$ initial) _rbAttrMap
  pure (events, app)

data ReflexBrickApp t n =
  ReflexBrickApp {
    rbaContinue         :: Event t (ReflexBrickAppState n)
  , rbaSuspendAndResume :: Event t (IO (ReflexBrickAppState n))
  , rbaHalt             :: Event t ()
  }

switchReflexBrickApp :: Reflex t
                     => Dynamic t (ReflexBrickApp t n)
                     -> ReflexBrickApp t n
switchReflexBrickApp d =
  ReflexBrickApp
    (switchDyn $ rbaContinue <$> d)
    (switchDyn $ rbaSuspendAndResume <$> d)
    (switchDyn $ rbaHalt <$> d)

nextEvent :: (Reflex t, MonadHold t m)
          => ReflexBrickAppState n
          -> Event t a
          -> ReflexBrickApp t n
          -> m (Event t (RBNext (ReflexBrickAppState n)))
nextEvent i eIn (ReflexBrickApp eC eSR eH) = do
  dState <- holdDyn i eC
  pure . leftmost $ [
      RBHalt <$> current dState <@ eH
    , RBSuspendAndResume <$> eSR
    , RBContinue <$> eC
    , RBContinue <$> current dState <@ eIn
    ]

rbEventSelector :: Reflex t => Event t (BrickEvent n e) -> EventSelector t (RBEvent n e)
rbEventSelector =
  fan .
  fmap ((\(k :=> v) -> singleton k v) . brickEventToRBEvent)

runReflexBrickApp :: Ord n
                  => EventM n e
                  -- ^ An initial action to perform
                  -> Maybe (IO e)
                  -- ^ An action which provides values for the custom event
                  -> ReflexBrickAppState n
                  -- ^ The initial state of the application
                  -> (forall t m. BasicGuestConstraints t m
                      => EventSelector t (RBEvent n e)
                      -> BasicGuest t m (ReflexBrickApp t n))
                  -- ^ The FRP network for the application
                  -> IO ()
runReflexBrickApp initial mGenE initialState fn = do
  (events, app) <- mkReflexApp initial
  basicHostWithQuit $ do
    (eQuit, onQuit) <- newTriggerEvent
    (eEventIn, onEventIn) <- newTriggerEvent
    mbChan <- fromMaybe (pure Nothing) $ (fmap Just . liftIO . newBChan $ 1) <$ mGenE

    void . liftIO . forkIO $ do
      void $ customMain (V.mkVty V.defaultConfig) mbChan app initialState
      onQuit ()

    let
      generate bChan genE = void . liftIO . forkIO . forever $ do
        e <- genE
        writeBChan bChan e
    fromMaybe (pure ()) $ generate <$> mbChan <*> mGenE

    void . liftIO . forkIO . forever $ do
      e <- takeMVar (rbeToReflex events)
      onEventIn e

    rba <- fn (rbEventSelector eEventIn)
    eEventOut <- nextEvent initialState eEventIn rba
    performEvent_ $ liftIO . putMVar (rbeFromReflex events) <$> eEventOut

    pure ((), eQuit)
