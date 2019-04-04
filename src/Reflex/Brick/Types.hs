{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Brick.Types where

import Control.Lens.TH (makePrisms, makeLenses)

import Brick
import qualified Graphics.Vty as V

data RBNext s
  = RBContinue
  | RBHalt
  | RBSuspendAndResume (IO s)

makePrisms ''RBNext

data ReflexBrickAppState n
  = ReflexBrickAppState
  { _rbWidgets  :: [Widget n]
  , _rbCursorFn :: [CursorLocation n] -> Maybe (CursorLocation n)
  , _rbAttrMap  :: AttrMap
  }

makeLenses ''ReflexBrickAppState

emptyState :: ReflexBrickAppState n
emptyState =
  ReflexBrickAppState [] (const Nothing) (attrMap V.defAttr [])

