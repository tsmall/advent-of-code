import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Data.Map.Strict (Map, (!?))
import Data.Sequence (Seq(..))
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main =
  do
    input <- load "example.txt"
    putStrLn $ "Part 1: " ++ "TODO" -- (show $ part1 input)
    putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 input)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/17/" ++ fileName


-- -----------------------------------------------------------------------------
-- Data types


data Graph = Graph
  { graphWidth :: Int
  , graphHeight :: Int
  }
  deriving (Show)


data Point = Point
  { pointX :: Int
  , pointY :: Int
  }
  deriving (Show, Eq, Ord)


-- -----------------------------------------------------------------------------
-- Helpers


bfs :: Graph -> Point -> Point -> [Point]
bfs graph start goal =
  loop (Seq.singleton start) Map.empty
  where
    loop frontier cameFrom =
      case frontier of
        Empty ->
          pathBack cameFrom goal start

        (current :<| frontier')
          | current == goal ->
            pathBack cameFrom goal start
          | otherwise ->
            loop newFrontier newCameFrom

          where
            (newFrontier, newCameFrom) =
              foldl update (frontier', cameFrom)
                $ filter notSeen
                $ neighbors graph current

            update (frontier'', cameFrom') next =
              ( frontier'' :|> next
              , Map.insert next current cameFrom'
              )

            notSeen point =
              not $ Map.member point cameFrom


pathBack :: Map Point Point -> Point -> Point -> [Point]
pathBack cameFrom goal start =
  loop [] (Just goal)
  where
    loop path current =
      case current of
        Nothing ->
          path

        Just current
          | current == start ->
            path
          | otherwise ->
            loop (current : path) (cameFrom !? current)


neighbors :: Graph -> Point -> [Point]
neighbors graph current =
  filter inBounds possiblePoints
  where
    inBounds (Point x y) =
      (x >= 0) && (x <= width-1) && (y >= 0) && (y <= height-1)

    Graph width height =
      graph

    possiblePoints =
      -- TODO: This should only return the point in front of current
      --       or the points to its side. But to do that, I first need
      --       to switch from Point to Vector.
      let
        Point x y =
          current

      in
        [ Point x (y+1)
        , Point x (y-1)
        , Point (x+1) y
        , Point (x-1) y
        ]
