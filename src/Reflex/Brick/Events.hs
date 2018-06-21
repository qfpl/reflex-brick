{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
module Reflex.Brick.Events (
    RBEvent(..)
  , MouseData(..)
  , brickEventToRBEvent
  ) where

import Data.Functor.Identity (Identity(..))

import Brick.Types (BrickEvent(..), Location(..))
import Graphics.Vty.Input.Events

import Data.Dependent.Sum (DSum(..))
import Data.GADT.Compare
import Data.GADT.Show

import Data.ByteString (ByteString)

data MouseData n =
  MouseData {
    _mdName :: n
  , _mdModifiers :: [Modifier]
  , _mdLocation :: Location
  }

data RBEvent n e a where
  RBAppEvent :: RBEvent n e e
  RBKey :: Key -> RBEvent n e [Modifier]
  RBResize :: RBEvent n e Location
  RBPaste :: RBEvent n e ByteString
  -- RBFocus :: RBEvent n e Bool
  RBMouseDown :: Button -> RBEvent n e (MouseData n)
  RBMouseUp :: Maybe Button -> RBEvent n e (MouseData n)

instance GEq (RBEvent n e) where
  geq RBAppEvent RBAppEvent =
    Just Refl
  geq (RBKey k1) (RBKey k2) =
    if k1 == k2 then Just Refl else Nothing
  geq RBResize RBResize =
    Just Refl
  geq RBPaste RBPaste =
    Just Refl
  -- geq RBFocus RBFocus =
  --   Just Refl
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
  gcompare (RBKey k1) (RBKey k2) =
    case compare k1 k2 of
      EQ -> GEQ
      LT -> GLT
      GT -> GGT
  gcompare (RBKey _) _ = GLT
  gcompare _ (RBKey _) = GGT
  gcompare RBResize RBResize = GEQ
  gcompare RBResize _ = GLT
  gcompare _ RBResize = GGT
  gcompare RBPaste RBPaste = GEQ
  gcompare RBPaste _ = GLT
  gcompare _ RBPaste = GGT
  -- gcompare RBFocus RBFocus = GEQ
  -- gcompare RBFocus _ = GLT
  -- gcompare _ RBFocus = GGT
  gcompare (RBMouseDown b1) (RBMouseDown b2) =
    case compare b1 b2 of
      EQ -> GEQ
      LT -> GLT
      GT -> GGT
  gcompare (RBMouseDown _) _ = GLT
  gcompare _ (RBMouseDown _) = GGT
  gcompare (RBMouseUp mb1) (RBMouseUp mb2) =
    case compare mb1 mb2 of
      EQ -> GEQ
      LT -> GLT
      GT -> GGT

instance GShow (RBEvent n e) where
  gshowsPrec _ RBAppEvent =
    showString "AppEvent"
  gshowsPrec n (RBKey k) =
    showString "Key " . showsPrec n k
  gshowsPrec _ RBResize =
    showString "Resize"
  gshowsPrec _ RBPaste =
    showString "Paste"
  -- gshowsPrec _ RBFocus =
  --   showString "Focus"
  gshowsPrec n (RBMouseDown b) =
    showString "MouseDown " . showsPrec n b
  gshowsPrec n (RBMouseUp mb) =
    showString "MouseUp " . showsPrec n mb

brickEventToRBEvent :: BrickEvent n e -> DSum (RBEvent n e) Identity
brickEventToRBEvent be =
  case be of
    AppEvent e ->
      RBAppEvent :=> Identity e
    VtyEvent e -> case e of
      EvKey k ms ->
        RBKey k :=> Identity ms
      EvResize x y ->
        RBResize :=> Identity (Location (x, y))
      EvPaste bs ->
        RBPaste :=> Identity bs
      -- EvGainedFocus ->
      --   RBFocus :=> Identity True
      -- EvLostFocus ->
      --   RBFocus :=> Identity False
      EvMouseDown _ _ _ _ ->
        error "EvMouseDown should be handled by brick"
      EvMouseUp _ _ _ ->
        error "EvMouseUp should be handled by brick"
    MouseDown n b ms l ->
      RBMouseDown b :=> Identity (MouseData n ms l)
    MouseUp n mb l ->
      RBMouseUp mb :=> Identity (MouseData n [] l)
