{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Brick.Events
  ( RBEvent(..)
  , RBKey(..)
  , MouseData(..)
  , brickEventToRBEvent
  )
where

import Data.Functor.Identity (Identity(..))

import Brick.Types (BrickEvent(..), Location(..))
import Graphics.Vty.Input.Events

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum ((==>))
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..), (:~:)(..))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show (GShow(..))
import Data.GADT.Show.TH (deriveGShow)

import qualified Data.Dependent.Map as DMap

import Data.ByteString (ByteString)
import Text.Show (showParen)

data MouseData n =
  MouseData {
    _mdName :: n
  , _mdModifiers :: [Modifier]
  , _mdLocation :: Location
  }

data RBKey a where
  RBEsc :: RBKey ()
  RBAnyChar :: RBKey Char
  RBChar :: Char -> RBKey ()
  RBBS :: RBKey ()
  RBEnter :: RBKey ()
  RBLeft :: RBKey ()
  RBRight :: RBKey ()
  RBUp :: RBKey ()
  RBDown :: RBKey ()
  RBUpLeft :: RBKey ()
  RBUpRight :: RBKey ()
  RBDownLeft :: RBKey ()
  RBDownRight :: RBKey ()
  RBCenter :: RBKey ()
  RBAnyFun :: RBKey Int
  RBFun :: Int -> RBKey ()
  RBBackTab :: RBKey ()
  RBPrtScr :: RBKey ()
  RBPause :: RBKey ()
  RBIns :: RBKey ()
  RBHome :: RBKey ()
  RBPageUp :: RBKey ()
  RBDel :: RBKey ()
  RBEnd :: RBKey ()
  RBPageDown :: RBKey ()
  RBBegin :: RBKey ()
  RBMenu :: RBKey ()
deriveGEq ''RBKey
deriveGCompare ''RBKey
deriveGShow ''RBKey

data RBEvent n e a where
  RBAppEvent :: RBEvent n e e
  RBKey :: RBEvent n e ([Modifier], DMap RBKey Identity)
  RBResize :: RBEvent n e Location
  RBPaste :: RBEvent n e ByteString
  RBMouseDown :: Button -> RBEvent n e (MouseData n)
  RBMouseUp :: Maybe Button -> RBEvent n e (MouseData n)
  -- RBFocus :: RBEvent n e Bool

instance GEq (RBEvent n e) where
  geq RBAppEvent RBAppEvent =
    Just Refl
  geq RBKey RBKey = Just Refl
  geq RBResize RBResize = Just Refl
  geq RBPaste RBPaste = Just Refl
  geq (RBMouseDown b1) (RBMouseDown b2) =
    if b1 == b2 then Just Refl else Nothing
  geq (RBMouseUp mb1) (RBMouseUp mb2) =
    if mb1 == mb2 then Just Refl else Nothing
  geq _ _ =
    Nothing

instance GCompare (RBEvent n e) where
  gcompare RBAppEvent RBAppEvent = GEQ
  gcompare RBAppEvent _ = GLT
  gcompare _ RBAppEvent = GGT
  gcompare RBKey RBKey = GEQ
  gcompare RBKey _ = GLT
  gcompare _ RBKey = GGT
  gcompare RBResize RBResize = GEQ
  gcompare RBResize _ = GLT
  gcompare _ RBResize = GGT
  gcompare RBPaste RBPaste = GEQ
  gcompare RBPaste _ = GLT
  gcompare _ RBPaste = GGT
  gcompare (RBMouseDown a) (RBMouseDown b) =
    case compare a b of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
  gcompare RBMouseDown{} _ = GLT
  gcompare _ RBMouseDown{} = GGT
  gcompare (RBMouseUp a) (RBMouseUp b) =
    case compare a b of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT

instance GShow (RBEvent n e) where
  gshowsPrec _ RBAppEvent =
    showString "AppEvent"
  gshowsPrec _ RBKey =
    showString "Key"
  gshowsPrec _ RBResize =
    showString "Resize"
  gshowsPrec _ RBPaste =
    showString "Paste"
  gshowsPrec n (RBMouseDown b) =
    showString "MouseDown " . showsPrec n b
  gshowsPrec n (RBMouseUp mb) =
    showString "MouseUp " . showsPrec n mb

keyToRBKey :: Key -> DMap RBKey Identity
keyToRBKey k =
  case k of
    KEsc -> DMap.singleton RBEsc $ pure ()
    KChar c -> DMap.fromList [RBAnyChar ==> c, RBChar c ==> ()]
    KBS -> DMap.singleton RBBS $ pure ()
    KEnter -> DMap.singleton RBEnter $ pure ()
    KLeft -> DMap.singleton RBLeft $ pure ()
    KRight -> DMap.singleton RBRight $ pure ()
    KUp -> DMap.singleton RBUp $ pure ()
    KDown -> DMap.singleton RBDown $ pure ()
    KUpLeft -> DMap.singleton RBUpLeft $ pure ()
    KUpRight -> DMap.singleton RBUpRight $ pure ()
    KDownLeft -> DMap.singleton RBDownLeft $ pure ()
    KDownRight -> DMap.singleton RBDownRight $ pure ()
    KCenter -> DMap.singleton RBCenter $ pure ()
    KFun n -> DMap.fromList [RBAnyFun ==> n, RBFun n ==> ()]
    KBackTab -> DMap.singleton RBBackTab $ pure ()
    KPrtScr -> DMap.singleton RBPrtScr $ pure ()
    KPause -> DMap.singleton RBPause $ pure ()
    KIns -> DMap.singleton RBIns $ pure ()
    KHome -> DMap.singleton RBHome $ pure ()
    KPageUp -> DMap.singleton RBPageUp $ pure ()
    KDel -> DMap.singleton RBDel $ pure ()
    KEnd -> DMap.singleton RBEnd $ pure ()
    KPageDown -> DMap.singleton RBPageDown $ pure ()
    KBegin -> DMap.singleton RBBegin $ pure ()
    KMenu -> DMap.singleton RBMenu $ pure ()

brickEventToRBEvent :: BrickEvent n e -> DMap (RBEvent n e) Identity
brickEventToRBEvent be =
  case be of
    AppEvent e ->
      DMap.singleton RBAppEvent (pure e)
    VtyEvent e ->
      case e of
        EvKey k ms ->
          DMap.singleton RBKey $ pure (ms, keyToRBKey k)
        EvResize x y ->
          DMap.singleton RBResize . pure $ Location (x, y)
        EvPaste bs ->
          DMap.singleton RBPaste $ pure bs
        -- EvGainedFocus ->
        --   RBFocus :=> Identity True
        -- EvLostFocus ->
        --   RBFocus :=> Identity False
        EvMouseDown _ _ _ _ ->
          error "EvMouseDown should be handled by brick"
        EvMouseUp _ _ _ ->
          error "EvMouseUp should be handled by brick"
    MouseDown n b ms l ->
      DMap.singleton (RBMouseDown b) (pure $ MouseData n ms l)
    MouseUp n mb l ->
      DMap.singleton (RBMouseUp mb) (pure $ MouseData n [] l)
