import qualified Data.Map.Strict as Map

import Data.Function ((&))
import Data.Map.Strict (Map, (!), (!?))
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main =
  do
    input <- load "input.txt"
    putStrLn $ "Part 1: " ++ (show $ part1 input)
    putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 input)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/14/" ++ fileName


part1 :: String -> Int
part1 input =
  platformLoad shiftedPlatform
  where
    shiftedPlatform =
      slideRocks platform

    platform =
      parseInput input


-- -----------------------------------------------------------------------------
-- Data types


data Platform = Platform
  { platformSize :: Int
  , platformMap :: PlatformMap
  }
  deriving (Show)


type PlatformMap =
  --    X   Y   Type
  Map (Int,Int) Char


-- -----------------------------------------------------------------------------
-- Helpers


platformLoad :: Platform -> Int
platformLoad (Platform size pm) =
  sum loads
  where
    loads =
      Map.elems $ Map.mapWithKey cellLoad pm

    cellLoad (x,y) char =
      case char of
        'O' ->
          size - y

        _ ->
          0


slideRocks :: Platform -> Platform
slideRocks (Platform size pm) =
  Platform size pm'
  where
    pm' =
      foldl (updateRow size) pm [0..size-1]


updateRow :: Int -> PlatformMap -> Int -> PlatformMap
updateRow size pm y =
  foldl (updateCell y) pm [0..size-1]


updateCell :: Int -> PlatformMap -> Int -> PlatformMap
updateCell y pm x =
  case (pm ! (x,y)) of
    'O' ->
      case (pm !? (x,y-1)) of
        Just '.' ->
          let
            pm' =
              Map.insert (x,y-1) 'O' pm & Map.insert (x,y) '.'
          in
            updateCell (y-1) pm' x

        _ ->
          pm

    _ ->
      pm


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> Platform
parseInput input =
  Platform size pm
  where
    pm =
      Map.fromList $ concat rows

    size =
      length rows

    rows =
      map parseLine $ zip [0..] $ lines input


parseLine :: (Int, String) -> [((Int,Int),Char)]
parseLine (y, line) =
  map parseChar $ zip [0..] line
  where
    parseChar (x,char) =
      ((x,y),char)
