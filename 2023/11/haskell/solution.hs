import qualified Data.Set as Set

import Data.List (tails)
import Data.Set (Set)
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 input)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/11/" ++ fileName


part1 :: String -> Int
part1 input =
  sum distances
  where
    distances =
      map getDistance galaxyPairs

    getDistance (pointA, pointB) =
      distanceBetween expandedSpace pointA pointB

    galaxyPairs =
      pairs $ filter isGalaxy $ concat spaceMap

    expandedSpace =
      findExpansions spaceMap

    spaceMap =
      parseInput input


-- -----------------------------------------------------------------------------
-- Data types


type SpaceMap =
  [[Point]]


data Point = Point
  { pointX :: Int
  , pointY :: Int
  , pointType :: PointType
  }
  deriving (Show)


data PointType
  = Space
  | Galaxy
  deriving (Show, Eq)


data ExpandedSpace = ExpandedSpace
  { expandedCols :: Set Int
  , expandedRows :: Set Int
  }
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


isGalaxy :: Point -> Bool
isGalaxy point =
  (pointType point) == Galaxy


pairs :: [a] -> [(a,a)]
pairs xs =
  [ (x, y) | (x:ys) <- tails xs, y <- ys ]


distanceBetween :: ExpandedSpace -> Point -> Point -> Int
distanceBetween expandedSpace pointA pointB =
  abs (xA - xB) + abs (yA - yB) + expansions
  where
    Point xA yA Galaxy =
      pointA

    Point xB yB Galaxy =
      pointB

    expansions =
      expansionsBetween expandedSpace pointA pointB


expansionsBetween :: ExpandedSpace -> Point -> Point -> Int
expansionsBetween (ExpandedSpace cols rows) pointA pointB =
  expandedColCount + expandedRowCount
  where
    expandedColCount =
      length $ filter (\x -> Set.member x cols) [startX .. endX]

    expandedRowCount =
      length $ filter (\y -> Set.member y rows) [startY .. endY]

    (startX, endX) =
      (min xA xB, max xA xB)

    (startY, endY) =
      (min yA yB, max yA yB)

    Point xA yA Galaxy =
      pointA

    Point xB yB Galaxy =
      pointB


findExpansions :: SpaceMap -> ExpandedSpace
findExpansions spaceMap =
  ExpandedSpace
    { expandedCols = Set.fromList expandedCols
    , expandedRows = Set.fromList expandedRows
    }
  where
    expandedCols =
      filter (isSpaceCol spaceMap) [0 .. maxX]

    expandedRows =
      filter (isSpaceRow spaceMap) [0 .. maxY]

    maxX =
      pointX $ last $ last spaceMap

    maxY =
      pointY $ head $ last spaceMap


isSpaceCol :: SpaceMap -> Int -> Bool
isSpaceCol spaceMap colX =
  all (== Space) pointTypes
  where
    pointTypes =
      map (\row -> pointType $ row !! colX) spaceMap


isSpaceRow :: SpaceMap -> Int -> Bool
isSpaceRow spaceMap colY =
  all (== Space) pointTypes
  where
    pointTypes =
      map pointType $ spaceMap !! colY


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> [[Point]]
parseInput str =
  map (\(line, y) -> parseLine y line) (zip (lines str) [0..])


parseLine :: Int -> String -> [Point]
parseLine y str =
  map (\(char, x) -> parsePoint x y char) (zip str [0..])


parsePoint :: Int -> Int -> Char -> Point
parsePoint x y char =
  Point x y pointType
  where
    pointType =
      case char of
        '.' -> Space
        '#' -> Galaxy
