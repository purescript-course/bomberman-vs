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
import Reactor.Graphics.Colors (red600)
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, ReactionM(..))

width :: Int
width = 15

height :: Int
height = 15

bombLimit :: Int
bombLimit = 1

ticksToExplode :: Int
ticksToExplode = 4

main :: Effect Unit
main = runReactor reactor { title: "Bomberman ", width, height }

data Tile = Wall | Empty | Bomb



type Bomb = {location :: Coordinates, time :: Int}

derive instance tileEq :: Eq Tile

type World = { player :: Coordinates, board :: Grid Tile, crates :: List Coordinates, bombsPlaced :: Int, bombs :: List Bomb}
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

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

initial :: World
initial = { player: { x: 1, y: 1 }, board, crates: (generateCrates), bombsPlaced: 0, bombs: Nil}
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
  updateBombs
  { player: { x, y }, board, crates } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    if elem newPlayerPosition crates then
      executeDefaultBehavior
    else
    updateW_ { player: newPlayerPosition }
  where
  isEmpty position board = Grid.index board position == Just Empty

placeBomb :: Reaction World
placeBomb = do
  updateBombs
  {player, board, bombsPlaced, bombs}  <- getW
  if bombsPlaced < bombLimit then
    updateW_ {board: (updateAt' player (Bomb ) board), bombsPlaced: bombsPlaced + 1, bombs: ({location: player, time: 0} : bombs)}
  else
    executeDefaultBehavior
    
 
updateBombs :: Reaction World
updateBombs = do
  {bombs} <- getW
  updateW_ {bombs:(map (\a -> if a.time < ticksToExplode then  {location: a.location, time: a.time + 1} else {location: a.location, time: a.time}) bombs) }
    
 -- foldr (\a b-> if a.time == ticksToExplode then b else a ) Nil bombs
  --filter (\a -> a.time < ticksToExplode) bombs
{-    
    if a.time == ticksToExplode 
    then do 
  --    updateW_ {board: (updateAt' a.location Empty board)}
      {location: a.location, time: a.time + 1} 
    else {location: a.location, time: a.time + 1}) bombs)}
-}

{-
updateBombsH :: List Bomb -> List Bomb
updateBombsH Nil = Nil
updateBombsH (Cons _ _) = Nil
updateBobmsH (Cons {location, time} r) = 
  if time >= ticksToExplode then 
    {board} <- getW
    updateW_ {board: (updateAt' location Empty board)}
    updateBombsH Nil
  else 
    {location: location, time: time + 1} : updateBobmsH r
    -}