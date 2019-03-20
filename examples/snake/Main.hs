{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main (
    main
  ) where

import Control.Monad (void)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Fix (MonadFix)
import Control.Concurrent (forkIO, threadDelay)
import Data.Bool (bool)

import Reflex
import Reflex.Workflow
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
import System.Random (newStdGen, randomR, randomRs)

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
  [ North <$ select es (RBVtyEvent $ RBKey V.KUp)
  , East  <$ select es (RBVtyEvent $ RBKey V.KRight)
  , South <$ select es (RBVtyEvent $ RBKey V.KDown)
  , West  <$ select es (RBVtyEvent $ RBKey V.KLeft)
  ]

selectRestart :: Reflex t
              => EventSelector t (RBEvent e n)
              -> Event t ()
selectRestart es =
  void . select es $ (RBVtyEvent . RBKey $ V.KChar 'r')

selectQuit :: Reflex t
           => EventSelector t (RBEvent e n)
           -> Event t ()
selectQuit es = void . leftmost $
  [ select es (RBVtyEvent . RBKey $ V.KChar 'q')
  , select es (RBVtyEvent $ RBKey V.KEsc)
  ]

genCoord :: MonadIO m => m Coord
genCoord = do
  r <- liftIO newStdGen
  let
    (x, r') = randomR (1, width) r
    (y, _) = randomR (1, height) r'
  pure $ V2 x y

genCoords :: MonadIO m => m [Coord]
genCoords =
  liftIO $ zipWith V2 <$> (randomRs (1, width) <$> newStdGen) <*> (randomRs (1, height) <$> newStdGen)

genFood :: MonadIO m => Seq Coord -> m Coord
genFood snake = do
  c <- genCoord
  if elem c snake then genFood snake else pure c

mkInitialState :: MonadIO m => m OutputState
mkInitialState = do
  let
    iSnake = Seq.singleton (V2 10 10)
  iFood <- genFood iSnake
  pure $ OutputState False 0 iSnake iFood

checkDirection :: Direction
               -> Direction
               -> Maybe Direction
checkDirection North South = Nothing
checkDirection East West = Nothing
checkDirection South North = Nothing
checkDirection West East = Nothing
checkDirection x y =
  if x == y then Nothing else Just y

mkMove ::
  forall t m.
  (Reflex t, MonadHold t m) =>
  Event t () ->
  Event t Direction ->
  Dynamic t Direction ->
  m (Event t Direction)
mkMove eTick eDirectionKey dDir = do
  bCanChange <- hold True $ leftmost [False <$ eDirectionKey, True <$ eTick]
  pure . fmapMaybe id $
    checkDirection <$>
    current dDir <@>
    gate bCanChange eDirectionKey

changePos :: Direction
          -> Coord
          -> Coord
changePos North = over _y succ
changePos East = over _x succ
changePos South = over _y pred
changePos West = over _x pred
changePos NoDir = id

mkHead ::
  (Reflex t, MonadFix m, MonadHold t m) =>
  Event t () ->
  Dynamic t Direction ->
  m (Dynamic t Coord)
mkHead eTick dDir =
  foldDyn ($) (V2 10 10) $ changePos <$> current dDir <@ eTick

mkTail :: (Reflex t, MonadFix m, MonadHold t m)
       => Dynamic t Bool
       -> Dynamic t Coord
       -> Event t ()
       -> m (Dynamic t (Seq Coord))
mkTail dAtFood dHead eTick =
  let
    grow b h t =
      bool (Seq.take (Seq.length t)) id b $ h Seq.<| t
  in
    foldDyn ($) mempty $
      grow <$> current dAtFood <*> current dHead <@ eTick

mkAtFood :: Reflex t
         => Dynamic t Coord
         -> Event t Coord
         -> (Event t Bool)
mkAtFood dFood eHead =
  (==) <$> current dFood <@> eHead

mkFood :: (Reflex t, MonadIO m, MonadFix m, MonadHold t m)
       => Coord
       -> Event t Bool
       -> m (Dynamic t Coord)
mkFood iFood eAtFood = do
  coords <- genCoords
  dFoods <- foldDyn (bool id tail) (iFood : coords) $ eAtFood
  pure $ head <$> dFoods

mkScore :: (Reflex t,  MonadHold t m, MonadFix m)
        => Event t Bool
        -> m (Dynamic t Int)
mkScore eAtFood =
  foldDyn ($) 0 $
    (+ 10) <$ ffilter id eAtFood

mkDead :: (Reflex t, MonadHold t m)
       => Dynamic t Coord
       -> Dynamic t (Seq Coord)
       -> m (Dynamic t Bool)
mkDead dPos dTail =
  let
    dDeadTail = elem <$> dPos <*> dTail
    atEdge p = p ^. _x <= 0 || p ^. _y <= 0 || p ^. _x > width || p ^. _y > height
    dDeadEdge = atEdge <$> dPos
  in
    holdDyn False . ffilter id . updated $ (||) <$> dDeadTail <*> dDeadEdge

isAlive ::
  (Reflex t, MonadHold t m, MonadFix m, PerformEvent t m, MonadIO (Performable m), MonadIO m) =>
  EventSelector t (RBEvent Name ()) ->
  Event t () -> -- ^ Tick event
  OutputState ->
  Workflow t m (ReflexBrickApp t Name)
isAlive es eTick initialState = Workflow $ do
  let
    eDirectionKey = selectDirection es
    eQuit = selectQuit es

  rec
    eMove <- mkMove eTick eDirectionKey dDir
    dDir <- holdDyn NoDir eMove
  dHead <- mkHead eTick dDir

  rec
    let
      eAtFood = mkAtFood dFood $ updated dHead
    dFood <- mkFood (_out_food initialState) eAtFood
  dAtFood <- holdDyn False eAtFood

  dTail <- mkTail dAtFood dHead eTick
  dDead <- mkDead dHead dTail
  dScore <- mkScore eAtFood

  let
    dSnake = (Seq.<|) <$> dHead <*> dTail
    dState = OutputState <$> dDead <*> dScore <*> dSnake <*> dFood

  pure
    ( ReflexBrickApp (renderState <$> dState) never eQuit
    , isDead es eTick <$ updated dDead
    )

isDead ::
  (Reflex t, MonadHold t m, MonadFix m, PerformEvent t m, MonadIO (Performable m), MonadIO m) =>
  EventSelector t (RBEvent Name ()) ->
  Event t () ->
  Workflow t m (ReflexBrickApp t Name)
isDead es eTick = Workflow $ do
  initialState <- mkInitialState
  let
    eRestart = selectRestart es
    eQuit = selectQuit es
  pure
    ( ReflexBrickApp (pure $ renderState initialState) never eQuit
    , isAlive es eTick initialState <$ eRestart
    )

main :: IO ()
main = do
  initialState <- mkInitialState
  runReflexBrickApp @() (pure ()) Nothing $ \es -> do
    (eTick, runTick) <- newTriggerEvent
    let
      ticking = do
        runTick ()
        threadDelay 300000
        ticking
    _ <- liftIO $ forkIO ticking
    switchReflexBrickApp <$> workflow (isAlive es eTick initialState)
