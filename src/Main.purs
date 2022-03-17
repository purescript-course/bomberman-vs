module Main
  where

import Debug
import Prelude

import Color (distance)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.Int as I
import Data.JSDate (getTime)
import Data.List (List(..), elem, filter, length, (:), delete, index, all)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Array as Array
import Data.Array ((!!))
import Data.Traversable (for_, for)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Effect.Random (randomInt)
import Halogen.HTML (elementNS, i)
import Halogen.HTML.Properties.ARIA (posInSet)
import Reactor (Reactor, Widget(..), executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, ReactionM, widget)
import Web.HTML.Event.BeforeUnloadEvent (returnValue)
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
ticksToExplode = 5

explosionDuration :: Int
explosionDuration = 1

tickSpeed :: Number
tickSpeed = 0.3

explosionLength :: Int
explosionLength = 4

main :: Effect Unit
main = do
  reactor <- createReactor
  runReactor reactor { title: "Bombermon ", width, height, widgets: [
  "section_hp" /\ Section {title: "Health"}, "label_hp" /\ Label {content: show $ 100}, "section_score" /\ Section {title: "Score"}, "label_score" /\ Label {content: show 0}]}

createReactor :: Effect (Reactor World)
createReactor = do
  initial <- createInitialWorld
  pure { initial, draw, handleEvent, isPaused: const false }

createInitialWorld :: Effect World
createInitialWorld = do
  board <- Grid.constructM width height constructor
  pure { player, enemies, board,lastTick: 0.0}
  where
  player = {location: { x: 1, y: 1 }, health: maxHealth, bombs: 0}
  enemies = [{location:{x: (width - 2), y: (height - 2)}, lastDirection: Nothing, bombs: 0}]
  constructor point 
    | isWall point = pure Wall
    | otherwise = do
      num <- (liftEffect (randomInt 0 2))
      if num == 1 then pure Crate else pure Empty

isWall :: Coordinates -> Boolean
isWall {x,y} =  x == 0 || x == (width - 1) || y == 0 || y == (height - 1) || (I.even x && I.even y)

data Tile = Wall | Empty | Crate | Bomb {timer :: Int} | Explosion {timer :: Int}
data Direction = Left | Right | Up | Down

type Enemy = {location :: Coordinates, lastDirection :: Maybe Coordinates, bombs :: Int}

type Player = {location :: Coordinates, health :: Int, bombs :: Int}

derive instance tileEq :: Eq Tile

type World = { player :: Player, enemies :: Array Enemy, board :: Grid Tile, lastTick :: Number}

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
  {lastTick, player} <- getW
  case event of
    Tick {delta} -> do
      if player.health <= 0 then 
        updateW_ {player: {location: {x: 1, y: 1}, health: 100, bombs: bombLimit}}
      else if lastTick >= tickSpeed then do 
        --updateBombs 
        updateW_ {lastTick: 0.0}
        moveEnemies
        
      else updateW_ {lastTick: lastTick + delta}
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " "} -> placeBomb
    _ -> executeDefaultBehavior

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  { player: {location:{ x, y }, health: health, bombs}, board} <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    updateW_ {player: {location: newPlayerPosition, health: health, bombs: bombs}}
  where
  isEmpty position board = Grid.index board position == Just Empty

getCor ∷ ∀ (t35 ∷ Type) (t39 ∷ Row Type). List { location ∷ t35 | t39 } → List t35
getCor Nil = Nil
getCor (Cons f r) = (f.location) : (getCor r)

placeBomb :: Reaction World
placeBomb = do
  {player: {location, bombs, health}, board}  <- getW
  if bombs <= bombLimit then
    updateW_ {player: {location: location, health: health, bombs: bombs + 1}, board: Grid.updateAt' location (Bomb {timer: ticksToExplode}) board} --
  else
    executeDefaultBehavior

{-updateBombs :: Reaction World
updateBombs = do
  updateExplosions
  {board} <- getW
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
  {board} <- getW
  updateW_ {inExplosion: (map (\b -> {location: b.location, time: b.time + 1}) (filter (\a -> a.time <= explosionDuration) inExplosion))}
-}
moveEnemies :: Reaction World
moveEnemies = do
  { board, player, enemies } <- getW
  movedEnemies <- liftEffect $ for enemies \e -> do
    shouldMove <- (_ < 8) <$> randomInt 0 10
    let bombermenLocations = Array.cons player.location (map (_.location) enemies)
    maybeDirection <- choice
      $ Array.filter (isJust <<< move board bombermenLocations e.location)
      $ Array.filter (\d -> (opposite d <$> e.lastDirection) /= Just true) directions
    pure $
      if shouldMove then fromMaybe (e { lastDirection = Nothing }) do
        dir <- maybeDirection
        loc <- move board bombermenLocations e.location dir
        pure $ e { location = loc, lastDirection = Just $ dir }
      else e
  updateW_ { enemies: movedEnemies }

  where
  directions = [ { x: -1, y: 0 }, { x: 1, y: 0 }, { x: 0, y: 1 }, { x: 0, y: -1 } ]

opposite :: Coordinates -> Coordinates -> Boolean
opposite { x: -1, y: 0 } { x: 1, y: 0 } = true
opposite { x: 1, y: 0 } { x: -1, y: 0 } = true
opposite { x: 0, y: 1 } { x: 0, y: -1 } = true
opposite { x: 0, y: -1 } { x: 0, y: 1 } = true
opposite _ _ = false


choice :: forall a. Array a -> Effect (Maybe a)
choice [] = pure Nothing
choice xs = (xs !! _) <$> randomInt 0 (Array.length xs - 1)

move :: Grid Tile -> Array Coordinates -> Coordinates -> Coordinates -> Maybe Coordinates
move board bombermen { x, y } { x: xd, y: yd } =
  if hasSpace && notOccupied then Just newLocation else Nothing

  where
  newLocation = { x: x + xd, y: y + yd }
  hasSpace = Grid.index board newLocation == Just Empty
  notOccupied = all (\loc -> loc /= newLocation) bombermen

{-
moveEnemies = do
  {crates, board, enemies} <- getW
  movedEnemies <- liftEffect $ for enemies \enemy -> do
    let possibleLocations = filter (\a -> a /= (fromMaybe {x: 0, y: 0} enemy.lastLocation) && (isEmpty a board)) ({x: enemy.x, y: (enemy.y - 1)} : {x: enemy.x, y: (enemy.y + 1)} : {x: (enemy.x - 1), y: enemy.y} : {x: (enemy.x + 1), y: enemy.y} : Nil)
    if possibleLocations == Nil then 
      enemy
    else --do
     -- let num = (length possibleLocations) - 1
    --  i <- (liftEffect (randomInt 0 num))
      --if i == num then do
        --updateW_ {bombs: (Cons {location: enemy, time: 0} bombs)}
    --  else 
      {position: fromMaybe {x: 0, y: 0} (index possibleLocations 0), lastLocation: enemy.location}
    where
    isEmpty position board = Grid.index board position == Just Empty
  updateW_ {enemies: movedEnemies}
-}
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
 --   else if fromMaybe (Bomb {timer: 0}) (Grid.index board {x,y}) == (Bomb {timer: _}) then do
  --    createExplosion {x, y}
    else do
      updateW_ {board: Grid.updateAt' {x: x,y: y} (Explosion {timer: explosionDuration}) board}
      case dir of
        Up -> do createExplosionH {x, y: y - 1} (dist + 1) dir
        Down -> do createExplosionH {x, y: y + 1} (dist + 1) dir
        Left -> do createExplosionH {x: x - 1, y} (dist + 1) dir
        Right -> do createExplosionH {x: x + 1, y} (dist + 1) dir