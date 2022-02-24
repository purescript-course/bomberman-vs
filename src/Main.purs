module Main
  where

import Debug
import Prelude

import Color (distance)
import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Int as I
import Data.JSDate (getTime)
import Data.List (List(..), elem, filter, length, (:), delete, index)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Random (randomInt)
import Halogen.HTML (elementNS, i)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, ReactionM)
import Web.TouchEvent.EventTypes (touchend)

width :: Int
width = 15

height :: Int
height = 15

bombLimit :: Int
bombLimit = 8

ticksToExplode :: Int
ticksToExplode = 5

tickSpeed :: Number
tickSpeed = 0.31 

explosionDuration :: Int
explosionDuration = 1

explosionLength :: Int
explosionLength = 4

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const false }

main :: Effect Unit
main = runReactor reactor { title: "B o m b e r m a n ", width, height }

data Tile = Wall | Empty

type Bomb = {location :: Coordinates, time :: Int}

data Direction = Left | Right | Up | Down

type Explosion = {location :: Coordinates, time :: Int}

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, enemy :: Coordinates, board :: Grid Tile, crates :: List Coordinates, bombs :: List Bomb, lastTick :: Number, cratesGenerated :: Boolean, inExplosion :: List Explosion, lastEnemyPosition :: Coordinates}

initial :: World
initial = { player: { x: 1, y: 1 }, board, crates: Nil, bombs: Nil, lastTick: 0.0, cratesGenerated: false, inExplosion: Nil, enemy: {x: (width - 2), y: (height - 2)}, lastEnemyPosition: {x: (width - 2), y: (height - 2)}}
  where
  board = Grid.construct width height (\point -> if isBorder point || generateWalls point then Wall else Empty)

isBorder :: Coordinates -> Boolean
isBorder { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1)

generateWalls :: Coordinates -> Boolean
generateWalls {x, y} = I.even x && I.even y
 
generateCrates :: Reaction World
generateCrates = generateCratesH 0 0
  where 
    generateCratesH x y = 
      if x == width then generateCratesH 0  $ y + 1  
      else if y == height then executeDefaultBehavior else
        if  (I.odd x && I.odd y) && ((x > 1) || (y > 1)) && (x < (width - 1)) && (y < (height - 1)) 
          then  do
            {crates} <- getW
            num <- (liftEffect (randomInt 0 1))
            if num == 1 then do
              updateW_{crates: ({x,y} : crates), cratesGenerated: true}
              generateCratesH (x + 1) y
            else
              generateCratesH (x + 1) y
        else  generateCratesH (x + 1) y

draw :: World -> Drawing
draw { player, board, crates, bombs, inExplosion, enemy } = do
  drawGrid board drawTile
  for_ crates $ \block -> fill Color.yellow400 $ tile block
  for_ bombs $ \block -> fill Color.red600 $ tile block.location
  for_ inExplosion $ \block -> fill Color.yellow600 $ tile block.location
  fill Color.blue500 $ tile player
  fill Color.green500 $ tile enemy
  where
  drawTile Empty = Just Color.green100
  drawTile Wall = Just Color.gray800

handleEvent :: Event -> Reaction World
handleEvent event = do
  {lastTick, cratesGenerated} <- getW
  case event of
    Tick {delta} ->  
      if lastTick >= tickSpeed then do 
        updateBombs 
        moveEnemy 
      else updateW_ {lastTick: lastTick + delta}
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> if not cratesGenerated then do 
      generateCrates 
      movePlayer { x: 1, y: 0 }
      else  movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> if not cratesGenerated  then do 
      generateCrates 
      movePlayer { x: 0, y: 1 }
      else  movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " "} -> placeBomb
    _ -> executeDefaultBehavior

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: { x, y }, board, crates, bombs } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  let bombsLocations = getCor bombs
  when (isEmpty newPlayerPosition board) $
    if elem newPlayerPosition crates || elem newPlayerPosition bombsLocations  then
      executeDefaultBehavior
    else
    updateW_ {player: newPlayerPosition}
  where
  isEmpty position board = Grid.index board position == Just Empty

getCor ∷ ∀ (t35 ∷ Type) (t39 ∷ Row Type). List { location ∷ t35 | t39 } → List t35
getCor Nil = Nil
getCor (Cons f r) = (f.location) : (getCor r)

placeBomb :: Reaction World
placeBomb = do
  {bombs, player}  <- getW
  if length bombs < bombLimit then
    updateW_ {bombs: (Cons {location: player, time: 0} bombs)}
  else
    executeDefaultBehavior

deleteBomb :: Coordinates -> Reaction World
deleteBomb location = do
  {bombs} <- getW
  deleteBombH location bombs
  where
  deleteBombH _ Nil = executeDefaultBehavior
  deleteBombH cor (f:r) = 
    if f.location == cor then do
      {bombs} <- getW
      updateW_ {bombs: (delete f bombs)}
    else 
      deleteBombH cor r

updateBombs :: Reaction World
updateBombs = do
  updateExplosions
  {bombs} <- getW
  updateBombsH bombs Nil
  where
  updateBombsH Nil acc = do updateW_ {bombs: acc, lastTick: 0.0}
  updateBombsH (f:r) acc =
    if f.time >= ticksToExplode then do
      createExplosion f.location  
      updateBombsH r acc      
    else
      updateBombsH r ({location: f.location, time: f.time + 1} : acc)

updateExplosions :: Reaction World
updateExplosions = do
  {inExplosion} <- getW
  updateW_ {inExplosion: (map (\b -> {location: b.location, time: b.time + 1}) (filter (\a -> a.time <= explosionDuration) inExplosion))}

moveEnemy :: Reaction World
moveEnemy = do
  {crates, board, enemy, bombs, lastEnemyPosition} <- getW
  let bombsLocations = getCor bombs
  let possibleLocations = filter (\a -> a /= lastEnemyPosition || (not (elem a crates) || not (elem a bombsLocations)) || (isEmpty a board)) ({x: enemy.x, y: (enemy.y - 1)} : {x: enemy.x, y: (enemy.y + 1)} : {x: (enemy.x - 1), y: enemy.y} : {x: (enemy.x + 1), y: enemy.y} : Nil)
  if possibleLocations == Nil then 
    executeDefaultBehavior
  else do
    let num = length possibleLocations
    i <- (liftEffect (randomInt 0 num))
    if i == num then do
      updateW_ {bombs: (Cons {location: enemy, time: 0} bombs)}
    else
      updateW_ {enemy: fromMaybe {x: 0, y: 0} (index possibleLocations i), lastEnemyPosition: enemy}
  where
  isEmpty position board = Grid.index board position == Just Empty


createExplosion :: Coordinates -> Reaction World
createExplosion cor = do
  deleteBomb cor
  updateW_ {inExplosion: ({location: {x:cor.x,y:cor.y}, time: 0}: Nil)}
  createExplosionH cor 0 Left 
  createExplosionH cor 0 Right
  createExplosionH cor 0 Down 
  createExplosionH cor 0 Up 
  where
  createExplosionH {x, y} dist dir = do
    {crates, board, inExplosion, bombs} <- getW
    let bombsLocations = getCor bombs
    if dist == explosionLength then executeDefaultBehavior   
    else if fromMaybe Wall (Grid.index board {x,y}) == Wall then 
      executeDefaultBehavior 
    else if elem {x,y} crates then do
      updateW_ {inExplosion: ({location: {x,y}, time: 0} : inExplosion), crates: (delete {x,y} crates)}
    else if elem {x,y} bombsLocations then do
      createExplosion {x, y}
    else do
      updateW_ {inExplosion: ({location: {x,y}, time: 0} : inExplosion)}
      case dir of
        Up -> do createExplosionH {x, y: y - 1} (dist + 1) dir
        Down -> do createExplosionH {x, y: y + 1} (dist + 1) dir
        Left -> do createExplosionH {x: x - 1, y} (dist + 1) dir
        Right -> do createExplosionH {x: x + 1, y} (dist + 1) dir