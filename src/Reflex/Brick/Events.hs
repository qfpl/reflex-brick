{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Brick.Events (
    RBEvent(..)
  , RBVtyEvent(..)
  , MouseData(..)
  , brickEventToRBEvent
  ) where

import Data.Functor.Identity (Identity(..))

import Brick.Types (BrickEvent(..), Location(..))
import Graphics.Vty.Input.Events

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum, (==>))
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

data RBVtyEvent a where
  RBAnyKey :: RBVtyEvent (Key, [Modifier])
  RBKey :: Key -> RBVtyEvent [Modifier]
  RBResize :: RBVtyEvent Location
  RBPaste :: RBVtyEvent ByteString

deriveGEq ''RBVtyEvent
deriveGCompare ''RBVtyEvent
deriveGShow ''RBVtyEvent

data RBEvent n e a where
  RBAppEvent :: RBEvent n e e
  RBAnyVtyEvent :: RBEvent n e (DMap RBVtyEvent Identity)
  RBVtyEvent :: RBVtyEvent a -> RBEvent n e a
  RBMouseDown :: Button -> RBEvent n e (MouseData n)
  RBMouseUp :: Maybe Button -> RBEvent n e (MouseData n)
  -- RBFocus :: RBEvent n e Bool

instance GEq (RBEvent n e) where
  geq RBAppEvent RBAppEvent =
    Just Refl
  geq RBAnyVtyEvent RBAnyVtyEvent = Just Refl
  geq (RBVtyEvent a) (RBVtyEvent b) = geq a b
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
  gcompare RBAnyVtyEvent RBAnyVtyEvent = GEQ
  gcompare RBAnyVtyEvent _ = GLT
  gcompare _ RBAnyVtyEvent = GGT
  gcompare (RBVtyEvent a) (RBVtyEvent b) = gcompare a b
  gcompare RBVtyEvent{} _ = GLT
  gcompare RBMouseDown{} RBVtyEvent{} = GGT
  gcompare (RBMouseDown a) (RBMouseDown b) =
    case compare a b of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
  gcompare RBMouseDown{} _ = GLT
  gcompare RBMouseUp{} RBVtyEvent{} = GGT
  gcompare RBMouseUp{} RBMouseDown{} = GGT
  gcompare (RBMouseUp a) (RBMouseUp b) =
    case compare a b of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT

instance GShow (RBEvent n e) where
  gshowsPrec _ RBAppEvent =
    showString "AppEvent"
  gshowsPrec _ RBAnyVtyEvent =
    showString "AnyVtyEvent"
  gshowsPrec n (RBVtyEvent a) =
    showParen (n > 10) $
    showString "VtyEvent " .
    gshowsPrec (n+1) a
  gshowsPrec n (RBMouseDown b) =
    showString "MouseDown " . showsPrec n b
  gshowsPrec n (RBMouseUp mb) =
    showString "MouseUp " . showsPrec n mb

brickEventToRBEvent :: BrickEvent n e -> DMap (RBEvent n e) Identity
brickEventToRBEvent be =
  case be of
    AppEvent e ->
      DMap.singleton RBAppEvent (pure e)
    VtyEvent e ->
      case e of
        EvKey k ms ->
          let
            key = RBKey k
            anyKey = RBAnyKey
            anyVal = (k, ms)
          in
            DMap.fromList
            [ RBVtyEvent anyKey ==> anyVal
            , RBVtyEvent key ==> ms
            , RBAnyVtyEvent ==>
              DMap.fromList
              [ anyKey ==> anyVal
              , key ==> ms
              ]
            ]
        EvResize x y ->
          let
            key = RBResize
            val = Location (x, y)
          in
            DMap.fromList
            [ RBVtyEvent key ==> val
            , RBAnyVtyEvent ==> DMap.singleton key (pure val)
            ]
        EvPaste bs ->
          let
            key = RBPaste
          in
            DMap.fromList
            [ RBVtyEvent key ==> bs
            , RBAnyVtyEvent ==> DMap.singleton key (pure bs)
            ]
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
