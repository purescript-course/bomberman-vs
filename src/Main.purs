module Main
  where

import Data.List
import Prelude

import Data.Grid (Grid, Coordinates, modifyAt, updateAt')
import Data.Grid as Grid
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
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
bombLimit = 1

main :: Effect Unit
main = runReactor reactor { title: "Bomberman ", width, height }
type Bomb = {time :: Int}

data Tile = Wall | Empty | Bomb

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile, crates :: List Coordinates, bombsPlaced :: Int }

isBorder :: Coordinates -> Boolean
isBorder { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1)

generateWalls :: Coordinates -> Boolean
generateWalls {x, y} = I.even x && I.even y

generateCrates :: List Coordinates
generateCrates = generateCratesH 0 0

generateCratesH :: Int -> Int -> List Coordinates
generateCratesH x y = 
  if x == width then generateCratesH 0  $ y + 1  
  else if y == height then Nil else
    if  (I.odd x && I.odd y) && ((x > 1) || (y > 1)) && (x < (width - 1)) && (y < (height - 1)) 
      then {x,y} : (generateCratesH (x + 1) y) 
    else  generateCratesH (x + 1) y

placeBomb :: Reaction World
placeBomb = do
  {player, board, bombsPlaced}  <- getW
  if bombsPlaced < bombLimit then
    updateW_ {board: (updateAt' player Bomb board), bombsPlaced: bombsPlaced + 1}
  else
    executeDefaultBehavior

--updateBomb :: Coordinates -> Reaction World

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x: 1, y: 1 }, board, crates: (generateCrates), bombsPlaced: 0}
  where
  board = Grid.construct width height (\point -> if isBorder point || generateWalls point then Wall else Empty)

draw :: World -> Drawing
draw { player, board, crates } = do
  drawGrid board drawTile
  for_ crates $ \block -> fill Color.yellow400 $ tile block
  fill Color.red600 $ tile player
  where
  drawTile Empty = Just Color.green100
  drawTile Wall = Just Color.gray800
  drawTile Bomb = Just Color.blue800


handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " "} -> placeBomb

    _ -> executeDefaultBehavior


movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: { x, y }, board, crates } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    if elem newPlayerPosition crates then
      executeDefaultBehavior
    else
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty
