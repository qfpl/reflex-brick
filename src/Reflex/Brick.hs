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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)

import Reflex
import Reflex.Host.Basic

import Brick.Types (BrickEvent(..), EventM)
import Brick.Main (App(..), customMain, continue, halt, suspendAndResume)
import Brick.BChan (newBChan, writeBChan)

import qualified Graphics.Vty as V

import Data.Functor ((<&>))
import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (singleton)

import Reflex.Brick.Types
import Reflex.Brick.Events

mkReflexApp ::
  MonadIO m =>
  EventM n e ->
  TVar (ReflexBrickAppState n) ->
  m
    ( MVar (BrickEvent n e)
    , App (ReflexBrickAppState n) (Either (RBNext (ReflexBrickAppState n)) e) n
    )
mkReflexApp initial state = do
  toReflex <- liftIO newEmptyMVar
  let
    process _ (AppEvent (Left e)) =
      case e of
        RBContinue -> liftIO (readTVarIO state) >>= continue
        RBHalt -> liftIO (readTVarIO state) >>= halt
        RBSuspendAndResume s -> suspendAndResume s
    process _ e = do
      s <-
        liftIO $ do
          putMVar toReflex $
            case e of
              AppEvent (Right a) -> AppEvent a
              AppEvent Left{} -> error "impossible"
              VtyEvent a -> VtyEvent a
              MouseDown a b c d -> MouseDown a b c d
              MouseUp a b c -> MouseUp a b c
          readTVarIO state
      continue s
    app = App _rbWidgets _rbCursorFn process (<$ initial) _rbAttrMap
  pure (toReflex, app)

data ReflexBrickApp t n
  = ReflexBrickApp
  { rbaAppState         :: Dynamic t (ReflexBrickAppState n)
  , rbaSuspendAndResume :: Event t (IO (ReflexBrickAppState n))
  , rbaHalt             :: Event t ()
  }

switchReflexBrickApp :: Reflex t
                     => Dynamic t (ReflexBrickApp t n)
                     -> ReflexBrickApp t n
switchReflexBrickApp d =
  ReflexBrickApp
    (d >>= rbaAppState)
    (switchDyn $ rbaSuspendAndResume <$> d)
    (switchDyn $ rbaHalt <$> d)

rbEventSelector :: Reflex t => Event t (BrickEvent n e) -> EventSelector t (RBEvent n e)
rbEventSelector =
  fan .
  fmap ((\(k :=> v) -> singleton k v) . brickEventToRBEvent)

runReflexBrickApp :: Ord n
                  => EventM n e
                  -- ^ An initial action to perform
                  -> Maybe (IO e)
                  -- ^ An action which provides values for the custom event
                  -> (forall t m. BasicGuestConstraints t m
                      => EventSelector t (RBEvent n e)
                      -> BasicGuest t m (ReflexBrickApp t n))
                  -- ^ The FRP network for the application
                  -> IO ()
runReflexBrickApp initial mGenE fn = do
  basicHostWithQuit $ do
    (eQuit, onQuit) <- newTriggerEvent
    (eEventIn, onEventIn) <- newTriggerEvent
    bChan <- liftIO $ newBChan 1
    case mGenE of
      Nothing -> pure ()
      Just genE -> do
        e <- liftIO genE
        liftIO . writeBChan bChan $ Right e

    rba <- fn (rbEventSelector eEventIn)
    initialState <- sample $ current (rbaAppState rba)
    stateVar <- liftIO $ newTVarIO initialState
    performEvent_ $
      updated (rbaAppState rba) <&> \s ->
      liftIO $ do
        () <- atomically $ writeTVar stateVar s
        writeBChan bChan (Left RBContinue)

    (brickEvent, app) <- mkReflexApp initial stateVar

    void . liftIO . forkIO . forever $ do
      e <- takeMVar brickEvent
      onEventIn e

    void . liftIO . forkIO $ do
      void $ customMain (V.mkVty V.defaultConfig) (Just bChan) app initialState
      onQuit ()

    performEvent_ $
      liftIO (writeBChan bChan $ Left RBHalt) <$ rbaHalt rba
    performEvent_ $
      liftIO . writeBChan bChan . Left . RBSuspendAndResume <$> rbaSuspendAndResume rba

    pure ((), eQuit)
