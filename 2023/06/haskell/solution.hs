import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "../../../../advent-of-code-problems/2023/06/input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


part1 :: String -> Int
part1 input =
  product waysToWin
  where
    waysToWin =
      length <$> winningTimes <$> races

    races =
      parseInputSeparated input


part2 :: String -> Int
part2 input =
  waysToWin
  where
    waysToWin =
      length (winningTimes race)

    race =
      parseInputCombined input


-- -----------------------------------------------------------------------------
-- Data types


data Race = Race
  { raceTime :: Time
  , raceRecord :: Distance
  }
  deriving (Show)


data Time =
  Time Int
  deriving (Show)


data Distance =
  Distance Int
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


winningTimes :: Race -> [Time]
winningTimes (Race (Time t) (Distance d)) =
  map Time $ filter beatsDistance [1 .. t-1]
  where
    beatsDistance time =
      ((t - time) * time) > d


-- -----------------------------------------------------------------------------
-- Parsing


parseInputSeparated :: String -> [Race]
parseInputSeparated input =
  zipWith Race times distances
  where
    times =
      Time <$> timeNumbers

    distances =
      Distance <$> distanceNumbers

    [timeNumbers, distanceNumbers] =
      parseLine <$> lines input

    parseLine =
      (map read) . (drop 1) . words


parseInputCombined :: String -> Race
parseInputCombined input =
  Race time distance
  where
    time =
      Time timeNumber

    distance =
      Distance distanceNumber

    [timeNumber, distanceNumber] =
      parseLine <$> lines input

    parseLine =
      read . concat . (drop 1) . words
