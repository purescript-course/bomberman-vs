
module Main
  where

import Data.List (List(..), elem, filter, length, (:))
import Prelude
import Data.Grid (Coordinates, Grid)
import Data.Grid as Grid
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)

width :: Int
width = 15

height :: Int
height = 15

bombLimit :: Int
bombLimit = 2

ticksToExplode :: Int
ticksToExplode = 4

tickSpeed :: Number
tickSpeed = 0.4

main :: Effect Unit
main = runReactor reactor { title: "Bomberman ", width, height }

data Tile = Wall | Empty

type Bomb = {location :: Coordinates, time :: Int}

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile, crates :: List Coordinates, bombs :: List Bomb, lastTick :: Number}
isBorder :: Coordinates -> Boolean
isBorder { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1)

generateWalls :: Coordinates -> Boolean
generateWalls {x, y} = I.even x && I.even y

generateCrates :: List Coordinates
generateCrates = generateCratesH 0 0
  where 
    generateCratesH x y = 
      if x == width then generateCratesH 0  $ y + 1  
      else if y == height then Nil else
        if  (I.odd x && I.odd y) && ((x > 1) || (y > 1)) && (x < (width - 1)) && (y < (height - 1)) 
          then {x,y} : (generateCratesH (x + 1) y) 
        else  generateCratesH (x + 1) y



reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const false }

initial :: World
initial = { player: { x: 1, y: 1 }, board, crates: (generateCrates), bombs: Nil, lastTick: 0.0}
  where
  board = Grid.construct width height (\point -> if isBorder point || generateWalls point then Wall else Empty)

draw :: World -> Drawing
draw { player, board, crates, bombs } = do
  drawGrid board drawTile
  for_ crates $ \block -> fill Color.yellow400 $ tile block
  for_ bombs $ \block -> fill Color.red600 $ tile block.location
  fill Color.blue500 $ tile player
  where
  drawTile Empty = Just Color.green100
  drawTile Wall = Just Color.gray800
  


handleEvent :: Event -> Reaction World
handleEvent event = do
  {lastTick} <- getW
  case event of
    Tick {delta} -> if lastTick >= tickSpeed then updateBombs else updateW_ {lastTick: lastTick + delta}
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " "} -> placeBomb

    _ -> executeDefaultBehavior


movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: { x, y }, board, crates, bombs } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  let bombsLocations = getBombsCor bombs
  when (isEmpty newPlayerPosition board) $
    if elem newPlayerPosition crates || elem newPlayerPosition bombsLocations  then
      executeDefaultBehavior
    else
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty


getBombsCor :: List Bomb -> List Coordinates
getBombsCor Nil = Nil
getBombsCor (Cons f r) = (f.location) : (getBombsCor r)

placeBomb :: Reaction World
placeBomb = do
  {bombs, player}  <- getW
  if length bombs < bombLimit then
    updateW_ {bombs: (Cons {location: player, time: 0} bombs)}
  else
    executeDefaultBehavior
    

updateBombs :: Reaction World
updateBombs = do
  {bombs} <- getW
  updateW_ {bombs: (filter (\a -> a.time < ticksToExplode) (map (\a -> {location: a.location, time: a.time + 1}) bombs )), lastTick: 0.0}