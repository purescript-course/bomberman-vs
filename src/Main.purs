module Main
  where

import Data.Array ((!!))
import Data.Array as Array
import Data.Grid (Coordinates, Grid, updateAt')
import Data.Grid as Grid
import Data.Int as I
import Data.List (List(..), all, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (for_, for)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Prelude (class Eq, Unit, bind, const, discard, map, mod, negate, otherwise, pure, show, when, ($), (&&), (+), (-), (/=), (<), (>), (<$>), (<<<), (<=), (==), (||))
import Reactor (Reactor, Widget(..), executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)
import Web.TouchEvent.EventTypes (touchend)


width :: Int
width = 15

height :: Int
height = 15

bombLimit :: Int
bombLimit = 5

maxHealth :: Int
maxHealth = 100

ticksToExplode :: Int
ticksToExplode = 6

explosionDuration :: Int
explosionDuration = 3

tickSpeed :: Int
tickSpeed = 25

explosionLength :: Int
explosionLength = 4

slidingSpeed :: Number
slidingSpeed = 0.1

main :: Effect Unit 
main = do
  reactor <- createReactor
  runReactor reactor { title: "Bombermqefffe ", width, height, widgets: [
  "section_hp" /\ Section {title: "Health"}, 
  "label_hp" /\ Label {content: show $ 100}, 
  "section_score" /\ Section {title: "Score"}, 
  "label_score" /\ Label {content: show 0}]}

createReactor :: Effect (Reactor World)
createReactor = do
  initial <- createInitialWorld
  pure { initial, draw, handleEvent, isPaused: const false }

createInitialWorld :: Effect World
createInitialWorld = do
  board <- Grid.constructM width height constructor
  pure { player, enemies, board,lastTick: 0}
  where
  player = {location: { x: 1, y: 1 }, health: maxHealth, bombs: 0}
  enemies = ({location:{x: (width - 2), y: (height - 2)}, lastDirection: {x: (width - 2), y: (height - 2)}, bombs: 0} : {location: {x: (width - 2), y: 1}, lastDirection: {x: (width - 2), y: 1}, bombs: 0} : Nil)
  constructor point 
    | isWall point = pure Wall
    | isCorner point = pure Empty
    | otherwise = do
        num <- (liftEffect (randomInt 0 1))
        if num == 1 then pure Crate else pure Empty

isCorner :: Coordinates -> Boolean
isCorner point = List.elem point ({x: 1, y: 1} : {x: 1, y: 2} : {x: 2, y: 1} : {x: (width - 2), y: 1} : {x: (width - 2), y: 2} : 
    {x: (width - 3), y: 1} : {x: 1, y: (height - 2)} : {x: 1, y: (height - 3)} : {x: 2, y: (height - 2)} : {x: (width - 2), y: (height - 2)} : 
    {x: (width - 2), y: (height - 3)} : {x: (width - 3), y: (height - 2)} : Nil)

isWall :: Coordinates -> Boolean
isWall {x,y} =  x == 0 || x == (width - 1) || y == 0 || y == (height - 1) || (I.even x && I.even y)

data Tile = Wall | Empty | Crate | Bomb {timer :: Int} | Explosion {timer :: Int}
data Direction = Left | Right | Up | Down

type Enemy = {location :: Coordinates, lastDirection :: Coordinates, bombs :: Int}

type Player = {location :: Coordinates, health :: Int, bombs :: Int}

derive instance tileEq :: Eq Tile

type World = { player :: Player, enemies :: List Enemy, board :: Grid Tile, lastTick :: Int}

draw :: World -> Drawing
draw { player, board, enemies} = do
  drawGrid board drawTile
  for_ enemies $ \block -> fill Color.green500 $ tile block.location
  fill Color.blue500 $ tile player.location
  where
  drawTile Empty = Just Color.green100
  drawTile Crate = Just Color.yellow300
  drawTile Wall = Just Color.gray800
  drawTile (Explosion _) = Just Color.yellow800
  drawTile (Bomb _) = Just Color.red500

handleEvent :: Event -> Reaction World
handleEvent event = do
  {player} <- getW
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " "} -> placeBomb
    Tick _ -> do
      if player.health <= 0 then 
        updateW_ {player: {location: {x: 1, y: 1}, health: 100, bombs: bombLimit}}
      else 
        timer
    _ -> executeDefaultBehavior

timer :: Reaction World
timer = do
  {lastTick} <- getW
  if (lastTick `mod` tickSpeed == 0) then do
    moveEnemies
    updateBombs
    updateExplosions
    updateW_ {lastTick: lastTick + 1} 
  else 
    updateW_ {lastTick: lastTick + 1}

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: {location:{ x, y }, health: health, bombs}, board} <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    updateW_ {player: {location: newPlayerPosition, health: health, bombs: bombs}}


placeBomb :: Reaction World
placeBomb = do
  {player: {location, bombs, health}, board}  <- getW
  if bombs <= bombLimit then
    updateW_ {player: {location: location, health: health, bombs: bombs + 1}, board: Grid.updateAt' location (Bomb {timer: ticksToExplode}) board}
  else
    executeDefaultBehavior

placeEnemyBomb :: Coordinates -> Reaction World
placeEnemyBomb cor = do
  {board} <- getW
  updateW_ {board: Grid.updateAt' cor (Bomb {timer: ticksToExplode}) board}

updateBombs :: Reaction World
updateBombs = do
  {board} <- getW
  let arrayGrid = Grid.enumerate board
  let bombs = Array.filter isBomb arrayGrid
  let bombsList = Array.foldl (\ a b -> b : a) Nil bombs
  updateBombsH bombsList
  where
  isBomb (Tuple _ (Bomb _)) = true
  isBomb (Tuple _ _) = false
  updateBombsH Nil = executeDefaultBehavior
  updateBombsH (Cons (Tuple _ Wall) _) = executeDefaultBehavior
  updateBombsH (Cons (Tuple _ Empty) _) = executeDefaultBehavior
  updateBombsH (Cons (Tuple _ Crate) _) = executeDefaultBehavior
  updateBombsH (Cons (Tuple _ (Explosion _)) _) = executeDefaultBehavior
  updateBombsH ((Tuple location (Bomb f)) : r) = 
    if f.timer == 0 then do
      {board} <- getW
      updateW_ {board: Grid.updateAt' location Empty board} 
      createExplosion location
      let newR = List.filter (\a -> (fst a) /= location ) r
      updateBombsH newR
    else do 
      {board} <- getW
      updateW_ {board: Grid.updateAt' location (Bomb {timer: (f.timer - 1)}) board}
      updateBombsH r

updateExplosions :: Reaction World
updateExplosions = do
  {board} <- getW
  let arrayGrid = Grid.enumerate board
  let explosions = Array.filter isExplosion arrayGrid
  let explosionsList = Array.foldl (\ a b -> b : a) Nil explosions
  updateExplosionsH explosionsList
  where
  isExplosion (Tuple _ (Explosion _)) = true
  isExplosion (Tuple _ _) = false
  updateExplosionsH Nil = executeDefaultBehavior
  updateExplosionsH (Cons (Tuple _ Wall) _) = executeDefaultBehavior
  updateExplosionsH (Cons (Tuple _ Empty) _) = executeDefaultBehavior
  updateExplosionsH (Cons (Tuple _ Crate) _) = executeDefaultBehavior
  updateExplosionsH (Cons (Tuple _ (Bomb _)) _) = executeDefaultBehavior
  updateExplosionsH ((Tuple location (Explosion f)) : r) = 
    if f.timer == 0 then do
      {board} <- getW
      updateW_ {board: Grid.updateAt' location Empty board} 
      updateExplosionsH r
    else do 
      {board} <- getW
      updateW_ {board: Grid.updateAt' location (Explosion {timer: (f.timer - 1)}) board}
      updateExplosionsH r

createExplosion :: Coordinates -> Reaction World
createExplosion cor = do
  {board} <- getW
  updateW_ {board: Grid.updateAt' {x:cor.x,y:cor.y} (Explosion {timer: explosionDuration}) board}
  createExplosionH cor 0 Left 
  createExplosionH cor 0 Right
  createExplosionH cor 0 Down 
  createExplosionH cor 0 Up 
  where
  createExplosionH {x, y} dist dir = do
    {board} <- getW
    if dist == explosionLength then executeDefaultBehavior   
    else if fromMaybe Wall (Grid.index board {x,y}) == Wall then 
      executeDefaultBehavior 
    else if fromMaybe Crate (Grid.index board {x,y}) == Crate then 
      updateW_ {board: Grid.updateAt' {x: x,y: y} (Explosion {timer: explosionDuration}) board}
    else if isBomb ( fromMaybe (Bomb {timer: 0}) (Grid.index board {x,y})) then do
      createExplosion {x, y}
    else do
      updateW_ {board: Grid.updateAt' {x: x,y: y} (Explosion {timer: explosionDuration}) board}
      case dir of
        Up -> do createExplosionH {x, y: y - 1} (dist + 1) dir
        Down -> do createExplosionH {x, y: y + 1} (dist + 1) dir
        Left -> do createExplosionH {x: x - 1, y} (dist + 1) dir
        Right -> do createExplosionH {x: x + 1, y} (dist + 1) dir
    where
    isBomb (Bomb _) = true
    isBomb _ = false

isEmpty :: { x :: Int, y :: Int} -> Grid Tile -> Boolean
isEmpty position board = Grid.index board position == Just Empty

moveEnemies :: Reaction World
moveEnemies = do
  {enemies} <- getW
  updateW_ {enemies: Nil}
  moveEnemiesH enemies 
  where
  moveEnemiesH Nil = executeDefaultBehavior
  moveEnemiesH (f:r) = do 
    {board, enemies} <- getW
    i <- (liftEffect (randomInt 0 3))
    if i /= 0 then do
        b <- (liftEffect (randomInt 0 8))
        if b == 5 then do
          placeEnemyBomb f.location
          updateW_ {enemies: (f : enemies)}
          moveEnemiesH r
        else do
          let locations = ({x: f.location.x, y: f.location.y + 1} : {x: f.location.x, y: f.location.y - 1} :
                            {x: f.location.x + 1, y: f.location.y} : {x: f.location.x - 1, y: f.location.y} : Nil)
          let possibleLocations = List.filter (\a -> isEmpty a board) locations
          let length = List.length possibleLocations
          if length == 1 then do
            updateW_ {enemies: ({location: fromMaybe {x: 0, y: 0} (List.head possibleLocations), bombs: f.bombs, lastDirection: f.location} : enemies)}
            moveEnemiesH r
          else if length > 1 then do
            ind <- (liftEffect (randomInt 0 (length - 2)))
            let withoutLast = List.filter (\a -> a /= f.lastDirection) possibleLocations
            updateW_ {enemies: ({location: fromMaybe {x: 0, y: 0} (List.index withoutLast ind), bombs: f.bombs, lastDirection: f.location} : enemies)}
            moveEnemiesH r
          else do
            updateW_ {enemies: (f : enemies)}
            moveEnemiesH r
    else do
      updateW_ {enemies: (f : enemies)}
      moveEnemiesH r