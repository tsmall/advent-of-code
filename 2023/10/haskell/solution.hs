import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "input.txt"
  let map = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 map)
  putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 map)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/10/" ++ fileName


part1 :: [[Pipe]] -> Int
part1 map =
  stepsUntilPointsMeet map startingVectors
  where
    startingVectors =
      ( Vector (Point 9 42) East
      , Vector (Point 8 43) South
      )


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
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


stepsUntilPointsMeet :: [[Pipe]] -> (Vector,Vector) -> Int
stepsUntilPointsMeet map points =
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
        pipe = pipeAt map point
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


pipeAt :: [[Pipe]] -> Point -> Pipe
pipeAt map (Point x y) =
  map !! y !! x


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
