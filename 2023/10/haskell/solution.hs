import Data.List (elemIndex)
import Data.Maybe (mapMaybe)
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "input.txt"
  let pipes = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 pipes)
  putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 pipes)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/10/" ++ fileName


part1 :: [[Pipe]] -> Int
part1 pipes =
  stepsUntilPointsMeet pipes startings
  where
    startings =
      startingVectors pipes


-- -----------------------------------------------------------------------------
-- Data types


data Vector =
  Vector Point Direction
  deriving (Show)


data Point = Point
  { pointX :: Int
  , pointY :: Int
  }
  deriving (Show, Eq)


data Direction
  = North
  | East
  | South
  | West
  deriving (Show)


data Pipe
  = Vertical
  | Horizontal
  | JointL
  | JointJ
  | Joint7
  | JointF
  | Ground
  | Start
  deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Helpers


stepsUntilPointsMeet :: [[Pipe]] -> (Vector,Vector) -> Int
stepsUntilPointsMeet pipes points =
  loop 1 points
  where
    loop steps (v1, v2) =
      let
        Vector point1 _ = v1
        Vector point2 _ = v2
      in
        if point1 == point2
        then
          steps
        else
          loop (steps + 1) ((move v1), (move v2))

    move vector =
      let
        Vector point _ = vector
        pipe = pipeAt pipes point
      in
        travelThrough pipe vector


travelThrough :: Pipe -> Vector -> Vector
travelThrough pipe (Vector point direction) =
  Vector newPoint newDirection
  where
    newPoint =
      point `adjustedBy` offset

    (offset, newDirection) =
      case (pipe, direction) of
        (Vertical, North) ->
          ((0, -1), North)

        (Vertical, South) ->
          ((0, 1), South)

        (Horizontal, East) ->
          ((1, 0), East)

        (Horizontal, West) ->
          ((-1, 0), West)

        (JointL, West) ->
          ((0, -1), North)

        (JointL, South) ->
          ((1, 0), East)

        (JointJ, East) ->
          ((0, -1), North)

        (JointJ, South) ->
          ((-1, 0), West)

        (Joint7, East) ->
          ((0, 1), South)

        (Joint7, North) ->
          ((-1, 0), West)

        (JointF, West) ->
          ((0, 1), South)

        (JointF, North) ->
          ((1, 0), East)


adjustedBy :: Point -> (Int, Int) -> Point
adjustedBy (Point x y) (offsetX, offsetY) =
  Point (x + offsetX) (y + offsetY)


adjustedByDirection :: Point -> Direction -> Point
adjustedByDirection point direction =
  point `adjustedBy` offset
  where
    offset =
      case direction of
        North -> (0, -1)
        East -> (1, 0)
        South -> (0, 1)
        West -> (-1, 0)


pipeAt :: [[Pipe]] -> Point -> Pipe
pipeAt pipes (Point x y) =
  pipes !! y !! x


pipeAtMaybe :: [[Pipe]] -> Point -> Maybe Pipe
pipeAtMaybe pipes (Point x y) =
   do
     row <- pipes !? y
     val <- row !? x
     return val


startingVectors :: [[Pipe]] -> (Vector, Vector)
startingVectors pipes =
  (vec1, vec2)
  where
    [vec1, vec2] =
      findVectors [North, East, South, West]

    findVectors directions =
      mapMaybe check directions

    check direction =
      let
        point = start `adjustedByDirection` direction
        vector = Vector point direction
      in
        case (direction, pipeAtMaybe pipes point) of
          (North, Just Vertical) -> Just vector
          (North, Just JointF) -> Just vector
          (North, Just Joint7) -> Just vector
          (East, Just Horizontal) -> Just vector
          (East, Just Joint7) -> Just vector
          (East, Just JointJ) -> Just vector
          (South, Just Vertical) -> Just vector
          (South, Just JointL) -> Just vector
          (South, Just JointJ) -> Just vector
          (West, Just Horizontal) -> Just vector
          (West, Just JointF) -> Just vector
          otherwise -> Nothing

    start =
      startingPoint pipes


startingPoint :: [[Pipe]] -> Point
startingPoint pipes =
  loop 0 pipes
  where
    loop y (row:rest) =
      case elemIndex Start row of
        Just index ->
          Point index y

        Nothing ->
          loop (y + 1) rest


-- This should be in the Data.List module according to the docs,
-- but it isn't in mine. (Maybe it was recently added?)
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 =
    Nothing
  | otherwise =
    foldr
      (\x r k -> case k of
                   0 -> Just x
                   _ -> r (k-1))
      (const Nothing) xs n


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> [[Pipe]]
parseInput input =
  map parseLine $ lines input


parseLine :: String -> [Pipe]
parseLine line =
  map parsePipe line


parsePipe :: Char -> Pipe
parsePipe char =
  case char of
    '|' -> Vertical
    '-' -> Horizontal
    'L' -> JointL
    'J' -> JointJ
    '7' -> Joint7
    'F' -> JointF
    '.' -> Ground
    'S' -> Start
