module Main where

import Data.List
import Prelude

import Data.Grid (Grid, Coordinates)
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

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile = Wall | Empty

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile, crates :: List Coordinates }

isBorder :: Coordinates -> Boolean
isBorder { x, y } = x == 0 || x == (width - 1) || y == 0 || y == (height - 1)

generateWalls :: Coordinates -> Boolean
generateWalls {x, y} = I.even x && I.even y

generateCrates :: Int -> Int -> List Coordinates
generateCrates x y = if x == width then generateCrates 0 (y + 1)  else if y == height then Nil else
  (if  (I.odd x && I.odd y) && ((x > 1) || (y > 1)) && (x < (width - 1)) && (y < (height - 1)) then {x,y} : (generateCrates (x + 1) y) else  (generateCrates (x + 1) y)) 

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x: 1, y: 1 }, board, crates: (generateCrates 0 0)}
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


handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }

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
