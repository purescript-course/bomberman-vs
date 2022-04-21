module Main
  where


import Data.Array as Array
import Data.Grid (Coordinates, Grid)
import Data.Grid as Grid
import Data.Int as I
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen.HTML (elementNS)
import Prelude (class Eq, Unit, bind, const, discard, mod, negate, otherwise, pure, show, ($), (&&), (*), (+), (-), (/=), (<=), (==), (>), (<), (||))
import Reactor (Reactor, Widget(..), executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, widget)

width :: Int
width = 15

height :: Int
height = 15

bombLimit :: Int
bombLimit = 4

bombStrength :: Int
bombStrength = 1

maxHealth :: Int
maxHealth = 100

ticksToExplode :: Int
ticksToExplode = 20

explosionDuration :: Int
explosionDuration = 8

tickSpeed :: Int
tickSpeed = 6

explosionLength :: Int
explosionLength = 4

enemySpeed :: Int
enemySpeed = 35
 
main :: Effect Unit 
main = do
  reactor <- createReactor
  runReactor reactor { title: "Bombermqeee ", width, height, widgets: [
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
  enemies = ({location:{x: (width - 2), y: (height - 2)}, lastDirection: {x: (width - 2), y: (height - 2)}, bombs: 0 , id: 0 , health: maxHealth} : 
    {location: {x: (width - 2), y: 1}, lastDirection: {x: (width - 2), y: 1}, bombs: 0, id: 1 , health: maxHealth} : Nil)
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

data Tile = Wall | Empty | Crate | Bomb {timer :: Int, slide :: Maybe Direction, owner :: Owner} | Explosion {timer :: Int, owner :: Owner}
data Direction = Left | Right | Up | Down

data Owner = Player | Enemy {id :: Int}

type Enemy = {location :: Coordinates, lastDirection :: Coordinates, bombs :: Int, id :: Int, health :: Int}

type Player = {location :: Coordinates, health :: Int, bombs :: Int}

derive instance tileEq :: Eq Tile
derive instance directionEq :: Eq Direction
derive instance ownerEq :: Eq Owner

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
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 } Left
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 } Right
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 } Down
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 } Up
    KeyPress { key: " "} -> placeBomb
    Tick _ -> do
        checkPlayer
        checkEnemies
        timer
    _ -> executeDefaultBehavior

timer :: Reaction World
timer = do
  {lastTick} <- getW
  if (lastTick `mod` enemySpeed == 0) then do
    moveEnemies
    updateBombs
    updateExplosions
    updateW_ {lastTick: lastTick + 1}  
  else if (lastTick `mod` tickSpeed == 0) then do
    updateBombs
    updateExplosions
    updateW_ {lastTick: lastTick + 1} 
  else 
    updateW_ {lastTick: lastTick + 1}

checkPlayer :: Reaction World
checkPlayer = do
  {player, board} <- getW
  if player.health <= 0 then do
    updateW_ {player: {location: {x: 1, y: 1}, health: 100, bombs: 0}}
    widget "label_hp" $ Label { content: show player.health }
  else if isExplosion (fromMaybe Empty (Grid.index board player.location)) then do
    updateW_ {player: {location: player.location, health: player.health - bombStrength, bombs: player.bombs}}
    widget "label_hp" $ Label { content: show player.health }
  else do
    executeDefaultBehavior
    widget "label_hp" $ Label { content: show player.health }

checkEnemies :: Reaction World 
checkEnemies = do 
  {enemies} <- getW 
  updateW_ {enemies: Nil}
  checkEnemiesH enemies 
  where 
  checkEnemiesH Nil = executeDefaultBehavior
  checkEnemiesH (f:r) = do 
    {enemies, board} <- getW
    if f.health <= 0 then do    
      checkEnemiesH r
    else if isPlayerExplosion (fromMaybe Empty (Grid.index board f.location)) then do
      updateW_ {enemies: {location: f.location, health: f.health - bombStrength, bombs: f.bombs, lastDirection: f.lastDirection, id: f.id} : enemies}
      checkEnemiesH r
    else do
       updateW_ {enemies: f : enemies}
       checkEnemiesH r
  isPlayerExplosion (Explosion {owner: Player, timer: _}) = true 
  isPlayerExplosion _ = false
    

movePlayer :: { x :: Int, y :: Int } -> Direction -> Reaction World
movePlayer { x: xd, y: yd } dir = do
  { player: {location:{ x, y }, health: health, bombs}, board} <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  if (isEmpty newPlayerPosition board) then
    updateW_ {player: {location: newPlayerPosition, health: health, bombs: bombs}}
  else if isBomb ( fromMaybe Empty (Grid.index board newPlayerPosition)) && (isEmpty { x: x + (2 * xd), y: y + (2 * yd) } board) then do 
    let bomb = fromMaybe Empty (Grid.index board newPlayerPosition)
    pushBomb newPlayerPosition { x: x + (2 * xd), y: y + (2 * yd) } dir bomb
  else 
    executeDefaultBehavior
  where
    isBomb (Bomb _) = true
    isBomb _ = false


pushBomb :: Coordinates -> Coordinates -> Direction -> Tile -> Reaction World 
pushBomb old new dir (Bomb bomb) = do 
  {board, player} <- getW 
  updateW_ {board: Grid.updateAt' old Empty board }
  {board} <- getW 
  updateW_ {player: {location: old, health: player.health, bombs: player.bombs}, board: Grid.updateAt' new (Bomb {timer: bomb.timer, slide: Just dir, owner: bomb.owner}) board }
pushBomb _ _ _ _ = executeDefaultBehavior


placeBomb :: Reaction World
placeBomb = do
  {player: {location, bombs}, board}  <- getW
  if bombs < bombLimit then do
    updateW_ {board: Grid.updateAt' location (Bomb {timer: ticksToExplode, slide: Nothing, owner: Player}) board}
    gainBomb Player (-1)
  else
    executeDefaultBehavior

placeEnemyBomb :: Coordinates -> Owner -> Reaction World
placeEnemyBomb cor owner = do
  {board} <- getW
  updateW_ {board: Grid.updateAt' cor (Bomb {timer: ticksToExplode, slide: Nothing, owner: owner}) board}
  gainBomb owner (-1)

gainBomb :: Owner -> Int -> Reaction World
gainBomb Player int = do
  {player} <- getW 
  updateW_ {player: {location: player.location, health: player.health, bombs: player.bombs - int}}
gainBomb (Enemy {id: id}) int = do 
  {enemies} <- getW
  updateW_ {enemies: gainBombEnemy enemies}
  where
  gainBombEnemy Nil = Nil
  gainBombEnemy (f:r) =
    if f.id == id then 
      {location: f.location, bombs: f.bombs - int, lastDirection: f.lastDirection, id: f.id, health: f.health} : r
    else 
      f : (gainBombEnemy r)


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
  updateBombsH ((Tuple location (Bomb f)) : r) = do
    {board} <- getW
    if f.timer <= 0 then do
      updateW_ {board: Grid.updateAt' location Empty board} 
      createExplosion location f.owner
      gainBomb f.owner 1
      let newR = List.filter (\a -> (fst a) /= location ) r
      updateBombsH newR
    else if f.slide == Nothing then do 
      updateW_ {board: Grid.updateAt' location (Bomb {timer: (f.timer - 1), slide: f.slide, owner: f.owner}) board}
      updateBombsH r
    else do
        case f.slide of
          Just Up -> do slide {x: location.x, y: location.y - 1}
          Just Down -> do slide {x: location.x, y: location.y + 1}
          Just Left -> do slide {x: location.x - 1, y: location.y}
          Just Right -> do slide {x: location.x + 1, y: location.y}
          Nothing -> do executeDefaultBehavior
    where
      slide cor = do 
        {board} <- getW
        if isEmpty cor board then do
          updateW_ {board: Grid.updateAt' location Empty board}
          let newR = List.filter (\a -> (fst a) /= location ) r
          {board} <- getW
          updateW_ {board: Grid.updateAt' cor (Bomb {timer: (f.timer - 1), slide: f.slide, owner: f.owner}) board}
          updateBombsH newR
        else do
          updateW_ {board: Grid.updateAt' location (Bomb {timer: (f.timer - 1), slide: Nothing, owner: f.owner}) board}
          updateBombsH r

updateExplosions :: Reaction World
updateExplosions = do
  {board} <- getW
  let arrayGrid = Grid.enumerate board
  let explosions = Array.filter isTupleExplosion arrayGrid
  let explosionsList = Array.foldl (\ a b -> b : a) Nil explosions
  updateExplosionsH explosionsList
  where
  isTupleExplosion (Tuple _ (Explosion _)) = true
  isTupleExplosion (Tuple _ _) = false
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
      updateW_ {board: Grid.updateAt' location (Explosion {timer: (f.timer - 1), owner: f.owner}) board}
      updateExplosionsH r

createExplosion :: Coordinates -> Owner -> Reaction World
createExplosion cor owner = do
  {board} <- getW
  updateW_ {board: Grid.updateAt' {x:cor.x,y:cor.y} (Explosion {timer: explosionDuration, owner: owner}) board}
  createExplosionH cor 0 Left owner
  createExplosionH cor 0 Right owner
  createExplosionH cor 0 Down owner 
  createExplosionH cor 0 Up owner
  where
  createExplosionH {x, y} dist dir owner = do
    {board} <- getW
    if dist == explosionLength then executeDefaultBehavior   
    else if fromMaybe Wall (Grid.index board {x,y}) == Wall then 
      executeDefaultBehavior 
    else if fromMaybe Crate (Grid.index board {x,y}) == Crate then 
      updateW_ {board: Grid.updateAt' {x: x,y: y} (Explosion {timer: explosionDuration, owner: owner}) board}
    else if isBomb ( fromMaybe (Bomb {timer: 0, slide: Nothing, owner: Player}) (Grid.index board {x,y})) then do
      let newOwner = (getOwner ( fromMaybe (Bomb {timer: 0, slide: Nothing, owner: Player}) (Grid.index board {x,y})))
      createExplosion {x, y} newOwner
      gainBomb newOwner 1
    else do
      updateW_ {board: Grid.updateAt' {x: x,y: y} (Explosion {timer: explosionDuration, owner: owner}) board}
      case dir of
        Up -> do createExplosionH {x, y: y - 1} (dist + 1) dir owner
        Down -> do createExplosionH {x, y: y + 1} (dist + 1) dir owner
        Left -> do createExplosionH {x: x - 1, y} (dist + 1) dir owner
        Right -> do createExplosionH {x: x + 1, y} (dist + 1) dir owner
    where
    isBomb (Bomb _) = true
    isBomb _ = false
    getOwner (Bomb bomb) = bomb.owner 
    getOwner _ = Player

isEmpty :: { x :: Int, y :: Int} -> Grid Tile -> Boolean
isEmpty position board = do 
  let tile = Grid.index board position
  tile == Just Empty || isExplosion (fromMaybe Empty tile)

isExplosion :: Tile -> Boolean 
isExplosion (Explosion _) = true
isExplosion _ = false

moveEnemies :: Reaction World
moveEnemies = do
  {enemies} <- getW
  updateW_ {enemies: Nil}
  moveEnemiesH enemies 
  where
  moveEnemiesH Nil = executeDefaultBehavior
  moveEnemiesH (f:r) = do 
    {board, enemies} <- getW
    i <- (liftEffect (randomInt 0 6))
    if i /= 0 then do
        b <- (liftEffect (randomInt 0 8))
        if (b == 5) && (f.bombs < bombLimit) then do
          placeEnemyBomb f.location (Enemy {id: f.id})
          updateW_ {enemies: (f : enemies)}
          moveEnemiesH r
        else do
          let locations = ({x: f.location.x, y: f.location.y + 1} : {x: f.location.x, y: f.location.y - 1} :
                          {x: f.location.x + 1, y: f.location.y} : {x: f.location.x - 1, y: f.location.y} : Nil)
          let possibleLocations = List.filter (\a -> isEmpty a board) locations
          let length = List.length possibleLocations
          if length == 1 then do
            updateW_ {enemies: ({location: fromMaybe {x: 0, y: 0} (List.head possibleLocations), bombs: f.bombs, lastDirection: f.location, id: f.id, health: f.health} : enemies)}
            moveEnemiesH r
          else if length > 1 then do
            ind <- (liftEffect (randomInt 0 (length - 2)))
            let withoutLast = List.filter (\a -> a /= f.lastDirection) possibleLocations
            updateW_ {enemies: ({location: fromMaybe {x: 0, y: 0} (List.index withoutLast ind), bombs: f.bombs, lastDirection: f.location, id: f.id, health: f.health} : enemies)}
            moveEnemiesH r
          else do
            updateW_ {enemies: (f : enemies)}
            moveEnemiesH r
    else do
      updateW_ {enemies: (f : enemies)}
      moveEnemiesH r