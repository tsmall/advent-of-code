module Main where

import Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main = do
  input <- readFile "../input.txt"
  let instructions = parseInstructions input
  putStrLn $ "Part 1: " ++ (show $ part1 instructions)
  putStrLn $ "Part 2: " ++ (show $ part2 instructions)


part1 :: Instructions -> Int
part1 instructions =
  manhattanDistance finalLocation
  where
    finalLocation = location finalPosition
    finalPosition = foldl move origin instructions


part2 :: Instructions -> Int
part2 instructions =
  manhattanDistance $ firstRepeatLocation instructions


type Instructions =
  [Instruction]


type Instruction =
  (Turn, Int)


data Direction
  = North
  | East
  | South
  | West
  deriving Show


data Turn
  = L
  | R
  deriving Show


data Point = Point
  { x :: Int
  , y :: Int
  }
  deriving (Show, Eq, Ord)


data Position = Position
  { heading  :: Direction
  , location :: Point
  }
  deriving Show


origin :: Position
origin = Position North (Point 0 0)


manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = (abs x) + (abs y)


move :: Position -> Instruction -> Position
move (Position heading point) (direction, steps) =
  Position newHeading newPoint
  where
    newHeading = turn direction heading
    newPoint   = last $ walk point newHeading steps


walk :: Point -> Direction -> Int -> [Point]
walk point North steps = map (\n -> point { y = (y point) + n }) [1..steps]
walk point East  steps = map (\n -> point { x = (x point) + n }) [1..steps]
walk point South steps = map (\n -> point { y = (y point) - n }) [1..steps]
walk point West  steps = map (\n -> point { x = (x point) - n }) [1..steps]


turn :: Turn -> Direction -> Direction
turn L North = West
turn R North = East
turn L East  = North
turn R East  = South
turn L South = East
turn R South = West
turn L West  = South
turn R West  = North


firstRepeatLocation :: Instructions -> Point
firstRepeatLocation instructions = firstRepeat instructions origin Set.empty


firstRepeat :: Instructions -> Position -> Set Point -> Point
firstRepeat [] position _ = error "No repeat found"
firstRepeat (inst:insts) position seen =
  let
    (Position heading location) = position
    (instruction, steps) = inst
    newHeading = turn instruction heading
    walked = walk location newHeading steps
    newLocation = last walked
    newPosition = Position newHeading newLocation
    newSeen = Set.fromList walked
    overlaps = Set.intersection seen newSeen
  in
    if Set.null overlaps then
      firstRepeat insts newPosition (Set.union seen newSeen)
    else
      head $ Set.toList overlaps


parseInstructions :: String -> Instructions
parseInstructions = map parseWord . (wordsOn isSeparator)


parseWord :: String -> Instruction
parseWord ('L':digit) = (L, read digit)
parseWord ('R':digit) = (R, read digit)


wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn pred str =
  case dropWhile pred str of
    "" ->
      []
    str' ->
      word : wordsOn pred rest
      where
        (word, rest) = break pred str'


isSeparator :: Char -> Bool
isSeparator ',' = True
isSeparator ' ' = True
isSeparator _   = False
