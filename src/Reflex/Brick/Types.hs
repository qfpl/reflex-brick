{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Brick.Types where

import Control.Lens.TH (makePrisms, makeLenses)

import Brick

data RBNext s =
    RBContinue s
  | RBHalt s
  | RBSuspendAndResume (IO s)

makePrisms ''RBNext

data ReflexBrickAppState n =
  ReflexBrickAppState {
    _rbWidgets  :: [Widget n]
  , _rbCursorFn :: [CursorLocation n] -> Maybe (CursorLocation n)
  , _rbAttrMap  :: AttrMap
  }

makeLenses ''ReflexBrickAppState

