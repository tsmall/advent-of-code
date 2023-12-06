import qualified Data.Set as Set

import Data.Maybe (fromMaybe, isJust)
import Data.List (find)
import System.IO (IOMode(..), openFile, hGetContents)
import Text.Parsec (ParseError, endBy1, many1, parse)
import Text.Parsec.Char (digit, spaces, string)
import Text.Parsec.String (Parser)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  -- inputText <- load "../../../../advent-of-code-problems/2023/05/example.txt"
  inputText <- load "../../../../advent-of-code-problems/2023/05/input.txt"
  case parseInput inputText of
    Left error ->
      print error

    Right input -> do
      putStrLn $ "Part 1: " ++ (show $ part1 input)
      -- putStrLn $ "Part 2: " ++ (show $ part2 input)
      putStrLn $ "Part 2: TODO"


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


part1 :: Input -> Int
part1 (Input seeds maps) =
  minimum locations
  where
    locations =
      map (chainLookup maps) seeds


part2 :: Input -> Int
part2 (Input seeds maps) =
  head locations
  where
    locations =
      filter hasSeed [1..]

    hasSeed location =
      Set.member (seedFromLocation location) actualSeeds

    seedFromLocation location =
      chainLookup reversedMaps location

    reversedMaps =
      reverseMaps maps

    actualSeeds =
      Set.fromList $ seedRanges [] seeds

    seedRanges result [] =
      result
    seedRanges result (start:len:rest) =
      seedRanges (result ++ [start .. start + len - 1]) rest


-- -----------------------------------------------------------------------------
-- Data types


data Input = Input
  { seeds :: [Int]
  , maps :: [Map]
  }
  deriving (Show)


data Map = Map
  { entries :: [MapEntry]
  }
  deriving (Show)


data MapEntry = MapEntry
  { destinationStart :: Int
  , sourceStart :: Int
  , rangeLength :: Int
  }
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


reverseMaps :: [Map] -> [Map]
reverseMaps maps =
  reverse $ map flipEntries maps
  where
    flipEntries (Map entries) =
      Map (map flipEntry entries)

    flipEntry (MapEntry destination source range) =
      MapEntry source destination range


chainLookup :: [Map] -> Int -> Int
chainLookup maps source =
  foldl mapLookup source maps


mapLookup :: Int -> Map -> Int
mapLookup source m =
  result `orElse` source
  where
    result =
      matchingEntry >>= doLookup

    matchingEntry =
      find (isJust . doLookup) (entries m)

    doLookup =
      entryLookup source


entryLookup :: Int -> MapEntry -> Maybe Int
entryLookup source entry =
  if
    (start <= source) && (source <= end)
  then
    Just $ (destinationStart entry) + (source - start)
  else
    Nothing
  where
    start =
      sourceStart entry

    end =
      start + (rangeLength entry) - 1


orElse :: Maybe a -> a -> a
orElse =
  flip fromMaybe


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> Either ParseError Input
parseInput input =
  parse inputParser "" input


inputParser :: Parser Input
inputParser = do
  seeds <- seedParser
  seedToSoilMap <- mapNamed "seed-to-soil map:"
  soilToFertilizerMap <- mapNamed "soil-to-fertilizer map:"
  fertilizerToWaterMap <- mapNamed "fertilizer-to-water map:"
  waterToLightMap <- mapNamed "water-to-light map:"
  lightToTemperatureMap <- mapNamed "light-to-temperature map:"
  temperatureToHumidityMap <- mapNamed "temperature-to-humidity map:"
  humidityToLocationMap <- mapNamed "humidity-to-location map:"
  return $ Input seeds [ seedToSoilMap
                       , soilToFertilizerMap
                       , fertilizerToWaterMap
                       , waterToLightMap
                       , lightToTemperatureMap
                       , temperatureToHumidityMap
                       , humidityToLocationMap
                       ]
  where
    mapNamed name = do
      spaces
      string name
      spaces
      m <- mapParser
      return m


seedParser :: Parser [Int]
seedParser =
  string "seeds: " *> (numberParser `endBy1` spaces)


mapParser :: Parser Map
mapParser = do
  entries <- entryParser `endBy1` spaces
  return $ Map entries


entryParser :: Parser MapEntry
entryParser = do
  destinationStart <- numberParser
  spaces
  sourceStart <- numberParser
  spaces
  rangeLength <- numberParser
  return $ MapEntry destinationStart sourceStart rangeLength


numberParser :: Parser Int
numberParser =
  read <$> many1 digit
