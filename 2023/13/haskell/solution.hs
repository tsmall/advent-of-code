import Data.List (find, transpose)
import Data.Maybe (fromJust)
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main =
  do
    input <- load "input.txt"
    let patterns = parseInput input
    putStrLn $ "Part 1: " ++ (show $ part1 patterns)
    putStrLn $ "Part 2: " ++ (show $ part2 patterns)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/13/" ++ fileName


part1 :: [[String]] -> Int
part1 patterns =
  sum $ map (mirrorScore Part1) patterns


part2 :: [[String]] -> Int
part2 patterns =
  sum $ map (mirrorScore Part2) patterns


-- -----------------------------------------------------------------------------
-- Data types


data Part
  = Part1
  | Part2


-- -----------------------------------------------------------------------------
-- Helpers


mirrorScore :: Part -> [String] -> Int
mirrorScore part rows =
  case mirrorLine part rows of
    Nothing ->
      fromJust $ mirrorLine part (transpose rows)

    Just line ->
      line * 100


mirrorLine :: Part -> [String] -> Maybe Int
mirrorLine part rows =
  find (isMirror part rows (reverse rows)) [1 .. length rows - 1]


isMirror :: Part -> [String] -> [String] -> Int -> Bool
isMirror part rows rowsReversed n =
  let
    before =
      take n $ drop (length rowsReversed - n) rowsReversed

    after =
      drop n rows

    diffs =
      map (\(xs, ys) -> differences xs ys) $ zip before after

  in
    case part of
      Part1 ->
        (sum diffs) == 0

      Part2 ->
        (sum diffs) == 1


differences :: String -> String -> Int
differences xs ys =
  countIf (== False) $ map (\(x, y) -> x == y) $ zip xs ys


countIf :: (a -> Bool) -> [a] -> Int
countIf pred xs =
  length $ filter pred xs


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> [[String]]
parseInput input =
  map lines $ paragraphs input


paragraphs :: String -> [String]
paragraphs string =
  loop [] "" string
  where
    loop result current s =
      case s of
        "" ->
          result ++ [reverse current]

        ('\n':'\n':rest) ->
          loop (result ++ [reverse current]) "" rest

        (char:rest) ->
          loop result (char:current) rest
