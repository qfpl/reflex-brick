{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
  ) where

import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Fix (MonadFix)
import Control.Concurrent (threadDelay)

import Reflex
import Reflex.Workflow
import Reflex.Network
import Reflex.Brick

import Brick hiding (Direction)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

import Control.Lens hiding (Empty)
import Linear.V2 (V2(..), _x, _y)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.Random (newStdGen, randomR)

data OutputState = OutputState
  { _out_dead :: Bool
  , _out_score :: Int
  , _out_snake :: Seq Coord
  , _out_food :: Coord
  }

type Coord = V2 Int
type Snake = Seq Coord

data Direction
  = North
  | South
  | East
  | West
  | NoDir
  deriving (Eq, Show)

height, width :: Int
height = 20
width = 20

type Name = ()

data Cell = Snake | Food | Empty

drawUI :: OutputState -> [Widget Name]
drawUI s = [C.center $ padRight (Pad 2) (drawStats s) <+> drawGrid s]

drawStats :: OutputState -> Widget Name
drawStats s = hLimit 11 $ vBox
  [drawScore (_out_score s), padTop (Pad 2) $ drawGameOver (_out_dead s)]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Score")
    $ C.hCenter
    $ padAll 1
    $ str
    $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver isDead = if isDead
  then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
  else emptyWidget

drawGrid :: OutputState -> Widget Name
drawGrid s =
  withBorderStyle BS.unicodeBold $ B.borderWithLabel (str "Snake") $ vBox rows
 where
  rows = [ hBox $ cellsInRow r | r <- [height, height - 1 .. 1] ]
  cellsInRow y = [ drawCoord (V2 x y) | x <- [1 .. width] ]
  drawCoord = drawCell . cellAt
  cellAt c | c `elem` _out_snake s = Snake
           | c == _out_food s      = Food
           | otherwise             = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr snakeAttr cw
drawCell Food  = withAttr foodAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (snakeAttr   , V.blue `on` V.blue)
  , (foodAttr    , V.red `on` V.red)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

snakeAttr, foodAttr, emptyAttr :: AttrName
snakeAttr = "snakeAttr"
foodAttr = "foodAttr"
emptyAttr = "emptyAttr"

renderState :: OutputState -> ReflexBrickAppState Name
renderState os = ReflexBrickAppState (drawUI os) (const Nothing) theMap

selectDirection :: Reflex t
                => EventSelector t (RBEvent e n)
                -> Event t Direction
selectDirection es = leftmost
  [ North <$ select es (RBKey V.KUp)
  , East  <$ select es (RBKey V.KRight)
  , South <$ select es (RBKey V.KDown)
  , West  <$ select es (RBKey V.KLeft)
  ]

selectRestart :: Reflex t
              => EventSelector t (RBEvent e n)
              -> Event t ()
selectRestart es =
  void . select es $ RBKey (V.KChar 'r')

selectQuit :: Reflex t
           => EventSelector t (RBEvent e n)
           -> Event t ()
selectQuit es = void . leftmost $
  [ select es (RBKey (V.KChar 'q'))
  , select es (RBKey V.KEsc)
  ]

atFood :: Reflex t
      => Dynamic t Coord
      -- ^ head position
      -> Dynamic t Coord
      -- ^ food position
      -> Event t ()
atFood dPos dFood =
  void . ffilter id . updated $
    (==) <$> dPos <*> dFood

score :: (Reflex t,  MonadHold t m, MonadFix m)
      => Event t ()
      -- ^ at food
      -> m (Dynamic t Int)
      -- ^ score
score eAtFood =
  foldDyn ($) 0 $
    (+ 10) <$ eAtFood

dead :: (Reflex t, MonadHold t m)
     => Dynamic t Coord
     -- ^ head position
     -> Dynamic t (Seq Coord)
     -- ^ tail position
     -> m (Dynamic t Bool)
     -- ^ whether or not the snake is dead
dead dPos dTail =
  let
    dDeadTail = elem <$> dPos <*> dTail
    atEdge p = p ^. _x <= 0 || p ^. _y <= 0 || p ^. _x > width || p ^. _y > height
    dDeadEdge = atEdge <$> dPos
  in
    holdDyn False . ffilter id . updated $ (||) <$> dDeadTail <*> dDeadEdge

changePos :: Direction
          -> Coord
          -> Coord
changePos North = over _y succ
changePos East = over _x succ
changePos South = over _y pred
changePos West = over _x pred
changePos NoDir = id

checkDirection :: Direction
               -> Direction
               -> Maybe Direction
checkDirection North South = Nothing
checkDirection East West = Nothing
checkDirection South North = Nothing
checkDirection West East = Nothing
checkDirection _ d = Just d

stopReverse :: (Reflex t, MonadHold t m, MonadFix m)
            => Event t Direction
            -> m (Dynamic t Direction, Event t Direction)
stopReverse eDirectionKey = mdo
  let eDirection = fmapMaybe id $ checkDirection <$> current dLastDirection <@> eDirectionKey
  dLastDirection <- holdDyn NoDir eDirection
  pure (dLastDirection, eDirection)

genCoord :: MonadIO m => m Coord
genCoord = do
  r <- liftIO newStdGen
  let
    (x, r') = randomR (1, width) r
    (y, _) = randomR (1, height) r'
  pure $ V2 x y

genFood' :: MonadIO m => Seq Coord -> m Coord
genFood' snake = do
  c <- genCoord
  if elem c snake
  then genFood' snake
  else pure c

genFood :: (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, MonadIO m) => Seq Coord -> Event t (Seq Coord) -> m (Dynamic t Coord)
genFood iSnake eSnake = do
  networkHold (genFood' iSnake) $ genFood' <$> eSnake

isAlive :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, Adjustable t m, MonadIO m)
        => EventSelector t (RBEvent Name ())
        -> Workflow t m (ReflexBrickApp t Name)
isAlive es = Workflow $ mdo
  let
    eTick = select es RBAppEvent
    eDirectionKey = selectDirection es
    eQuit = selectQuit es

  (dLastDirection, eDirection) <- stopReverse eDirectionKey

  let
    eMove = leftmost [eDirection, current dLastDirection <@ eTick]

  dHead <- foldDyn ($) (V2 10 10) $ changePos <$> eMove

  let dFood = pure (V2 5 5)
  let
    eAtFood = atFood dHead dFood

  dLastHead <- holdDyn mempty $ Seq.singleton <$> updated dHead
  dTail <- foldDyn ($) mempty .
           leftmost  $ [
             (\h t -> h Seq.>< t) <$> current dLastHead <@ eAtFood
           , (\h t -> Seq.take (Seq.length t) (h Seq.>< t)) <$> current dLastHead <@ eMove
           ]

  dDead <- dead dHead dTail
  dScore <- score eAtFood

  let
    dSnake = (Seq.<|) <$> dHead <*> dTail
    dState = OutputState <$> dDead <*> dScore <*> dSnake <*> dFood
    eOut = renderState <$> updated dState

  pure (ReflexBrickApp eOut never eQuit, isDead es <$ updated dDead)

isDead :: (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, Adjustable t m, MonadIO m)
       => EventSelector t (RBEvent Name ())
       -> Workflow t m (ReflexBrickApp t Name)
isDead es = Workflow $ do
  let
    eRestart = selectRestart es
    eQuit = selectQuit es
  pure (ReflexBrickApp (renderState initialState <$ eRestart) never eQuit, isAlive es <$ eRestart)

-- TODO feed this to isAlive
-- probably also requires random generation then
-- which is handy, can be used when restarting
initialState :: OutputState
initialState =
  OutputState False 0 (Seq.fromList [V2 10 10]) (V2 5 5)

main :: IO ()
main = do
  let
    tick = threadDelay 1000000
  runReflexBrickApp (pure ()) (Just tick) (renderState initialState) $
    fmap switchReflexBrickApp . workflow . isAlive
