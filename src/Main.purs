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
import Prelude (class Eq, Unit, bind, const, discard, mod, negate, otherwise, pure, show, ($), (&&), (*), (+), (-), (/=), (<), (<=), (==), (>), (||))
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
  runReactor reactor { title: "Bombera ", width, height, widgets: [
  "section_health" /\ Section {title: "Health"}, 
  "label_health" /\ Label {content: show $ 100}, 
  "section_score" /\ Section {title: "Score"}, 
  "label_score" /\ Label {content: show 0}]}

createReactor :: Effect (Reactor World)
createReactor = do
  initial <- createInitialWorld
  pure { initial, draw, handleEvent, isPaused: const false }

createInitialWorld :: Effect World
createInitialWorld = do
  board <- Grid.constructM width height constructor
  pure { player, enemies, board,lastTick: 0, score: 0}
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

derive instance tileEq :: Eq Tile
derive instance directionEq :: Eq Direction
derive instance ownerEq :: Eq Owner

data Tile = Wall | Empty | Crate | Bomb {timer :: Int, slide :: Maybe Direction, owner :: Owner} | Explosion {timer :: Int, owner :: Owner}
data Direction = Left | Right | Up | Down

data Owner = Player | Enemy {id :: Int}

type Enemy = {location :: Coordinates, lastDirection :: Coordinates, bombs :: Int, id :: Int, health :: Int}

type Player = {location :: Coordinates, health :: Int, bombs :: Int}

type World = { player :: Player, enemies :: List Enemy, board :: Grid Tile, lastTick :: Int, score :: Int}

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
  {player: p@{health}, board} <- getW
  if health <= 0 then do
    updateW_ {player: p {health = 100}, score: 0}
  else if isExplosion (fromMaybe Empty (Grid.index board p.location)) then do
    updateW_ {player: p {health = health - bombStrength}}
  else do
    executeDefaultBehavior
  widget "label_health" $ Label { content: show health }

checkEnemies :: Reaction World 
checkEnemies = do 
  {enemies} <- getW 
  updateW_ {enemies: Nil}
  checkEnemiesH enemies 
  where 
  checkEnemiesH Nil = executeDefaultBehavior
  checkEnemiesH ((f@{health}):r) = do 
    {enemies, board, score} <- getW
    if f.health <= 0 then do    
      checkEnemiesH r
    else if isPlayerExplosion (fromMaybe Empty (Grid.index board f.location)) then do
      updateW_ {enemies: f{health = health - bombStrength} : enemies, score: score + 1}
      checkEnemiesH r
    else do
       updateW_ {enemies: f : enemies}
       checkEnemiesH r 
    widget "label_score" $ Label { content: show score }
  isPlayerExplosion (Explosion {owner: Player, timer: _}) = true 
  isPlayerExplosion _ = false
    

movePlayer :: { x :: Int, y :: Int } -> Direction -> Reaction World
movePlayer { x: xd, y: yd } dir = do
  { player: p@{location:{x,y}}, board} <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  if (isEmpty newPlayerPosition board) then
    updateW_ {player: p{location = newPlayerPosition}}
  else if isBomb ( fromMaybe Empty (Grid.index board newPlayerPosition)) && (isEmpty { x: x + (2 * xd), y: y + (2 * yd) } board) then do 
    let bomb = fromMaybe Empty (Grid.index board newPlayerPosition)
    pushBomb newPlayerPosition { x: x + (2 * xd), y: y + (2 * yd) } dir bomb
  else 
    executeDefaultBehavior

pushBomb :: Coordinates -> Coordinates -> Direction -> Tile -> Reaction World 
pushBomb old new dir (Bomb (b@{slide})) = do 
  {board, player: p@{location}} <- getW 
  updateW_ {board: Grid.updateAt' old Empty board }
  {board} <- getW 
  updateW_ {player: p{location = old}, board: Grid.updateAt' new (Bomb b{slide = Just dir}) board }
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
  {player: p@{bombs}} <- getW 
  updateW_ {player: p{bombs = bombs - int}}
gainBomb (Enemy {id: id}) int = do 
  {enemies} <- getW
  updateW_ {enemies: gainBombEnemy enemies}
  where
  gainBombEnemy Nil = Nil
  gainBombEnemy (f@{bombs}:r) =
    if f.id == id then 
      (f {bombs = bombs - int}) : r
    else 
      f : (gainBombEnemy r)

updateBombs :: Reaction World
updateBombs = do
  {board} <- getW
  let arrayGrid = Grid.enumerate board
  let bombs = Array.filter isTupleBomb arrayGrid
  let bombsList = Array.foldl (\ a b -> b : a) Nil bombs
  updateBombsH bombsList
  where
  isTupleBomb (Tuple _ (Bomb _)) = true
  isTupleBomb (Tuple _ _) = false
  updateBombsH (Tuple location (Bomb (f@{timer,slide})) : r) = do
    {board} <- getW
    if f.timer <= 0 then do
      updateW_ {board: Grid.updateAt' location Empty board} 
      createExplosion location f.owner
      gainBomb f.owner 1
      let newR = List.filter (\a -> (fst a) /= location ) r
      updateBombsH newR
    else if f.slide == Nothing then do 
      updateW_ {board: Grid.updateAt' location (Bomb f{timer = timer - 1}) board}
      updateBombsH r
    else do
        case f.slide of
          Just Up -> do slideF {x: location.x, y: location.y - 1}
          Just Down -> do slideF {x: location.x, y: location.y + 1}
          Just Left -> do slideF {x: location.x - 1, y: location.y}
          Just Right -> do slideF {x: location.x + 1, y: location.y}
          Nothing -> do executeDefaultBehavior
    where
      slideF cor = do 
        {board} <- getW
        if isEmpty cor board then do
          updateW_ {board: Grid.updateAt' location Empty board}
          let newR = List.filter (\a -> (fst a) /= location ) r
          {board} <- getW
          updateW_ {board: Grid.updateAt' cor (Bomb f{timer = timer - 1}) board}
          updateBombsH newR
        else do
          updateW_ {board: Grid.updateAt' location (Bomb f{timer = f.timer - 1, slide = Nothing}) board}
          updateBombsH r
  updateBombsH _ = executeDefaultBehavior

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
  updateExplosionsH (Tuple location (Explosion (f@{timer})) : r) = 
    if f.timer == 0 then do
      {board} <- getW
      updateW_ {board: Grid.updateAt' location Empty board} 
      updateExplosionsH r
    else do 
      {board} <- getW
      updateW_ {board: Grid.updateAt' location (Explosion f{timer = timer - 1}) board}
      updateExplosionsH r
  updateExplosionsH _ = executeDefaultBehavior

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
      let newOwner = (getOwner ( fromMaybe (Bomb {timer: 0, slide: Nothing, owner: Player}) $ Grid.index board {x,y}))
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
    
    getOwner (Bomb bomb) = bomb.owner 
    getOwner _ = Player

isEmpty :: { x :: Int, y :: Int} -> Grid Tile -> Boolean
isEmpty position board = do 
  let tile = Grid.index board position
  tile == Just Empty || isExplosion (fromMaybe Empty tile)

isExplosion :: Tile -> Boolean 
isExplosion (Explosion _) = true
isExplosion _ = false

isBomb :: Tile -> Boolean
isBomb (Bomb _) = true
isBomb _ = false

moveEnemies :: Reaction World
moveEnemies = do
  {enemies} <- getW
  updateW_ {enemies: Nil}
  moveEnemiesH enemies 
  where
  moveEnemiesH Nil = executeDefaultBehavior
  moveEnemiesH ((f@{location}):r) = do 
    {board, enemies} <- getW
    i <- (liftEffect (randomInt 0 6))
    if i /= 0 then do
        b <- (liftEffect (randomInt 0 8))
        if (b == 5) && (f.bombs < bombLimit) then do
          placeEnemyBomb f.location (Enemy {id: f.id})
          updateW_ {enemies: (f : enemies)}
          moveEnemiesH r
        else do
          let locations = ({x: location.x, y: location.y + 1} : {x: location.x, y: location.y - 1} :
                          {x: location.x + 1, y: location.y} : {x: location.x - 1, y: location.y} : Nil)
          let possibleLocations = List.filter (\a -> isEmpty a board) locations
          let length = List.length possibleLocations
          if length == 1 then do
            updateW_ {enemies: f{location = fromMaybe {x: 0, y: 0}  $ List.head possibleLocations} : enemies}
            moveEnemiesH r
          else if length > 1 then do
            ind <- (liftEffect $ randomInt 0 $ length - 2)
            let withoutLast = List.filter (\a -> a /= f.lastDirection) possibleLocations
            updateW_ {enemies: f{location = fromMaybe {x: 0, y: 0} $ List.index withoutLast ind} : enemies}
            moveEnemiesH r
          else do
            updateW_ {enemies: (f : enemies)}
            moveEnemiesH r
    else do
      updateW_ {enemies: (f : enemies)}
      moveEnemiesH r