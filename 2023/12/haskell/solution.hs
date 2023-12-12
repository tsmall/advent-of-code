import Data.Function ((&))  -- TODO Delete Me!
import Data.Char (isDigit)
import Data.List (break, group, intersperse)
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
      "../../../../advent-of-code-problems/2023/12/" ++ fileName


part1 :: String -> Int
part1 input =
  sum arrangementCounts
  where
    arrangementCounts =
      map length arrangements

    arrangements =
      map (\(springs, sizes) -> validArrangements springs sizes) parsedLines

    parsedLines =
      map parseLine $ lines input


-- -----------------------------------------------------------------------------
-- Data types


data Spring
  = Unknown
  | Operational
  | Damaged
  deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Helpers


validArrangements :: [Spring] -> [Int] -> [[Spring]]
validArrangements springs sizes =
  filter (isValidArrangement sizes) (possibleArrangements springs)


isValidArrangement :: [Int] -> [Spring] -> Bool
isValidArrangement sizes springs =
  calculatedSizes == sizes
  where
    calculatedSizes =
      map length damagedGroups

    damagedGroups =
      filter (all (== Damaged)) groups

    groups =
      group springs


possibleArrangements :: [Spring] -> [[Spring]]
possibleArrangements [] =
  [[]]
possibleArrangements (spring:rest)
  | spring == Unknown =
    (possibleArrangements (Damaged:rest))
      ++ (possibleArrangements (Operational:rest))
  | otherwise =
    map (spring:) (possibleArrangements rest)


-- -----------------------------------------------------------------------------
-- Parsing


parseLine :: String -> ([Spring], [Int])
parseLine line =
  (springs, sizes)
  where
    springs =
      map parseSpring springsStr

    sizes =
      map read $ numbers sizesStr

    [springsStr, sizesStr] =
      words line


parseSpring :: Char -> Spring
parseSpring char =
  case char of
    '?' -> Unknown
    '.' -> Operational
    '#' -> Damaged


numbers :: String -> [String]
numbers str =
  case dropWhile (not . isDigit) str of
    "" ->
      []

    s ->
      num : numbers s'
      where
        (num, s') =
          break (not . isDigit) s

