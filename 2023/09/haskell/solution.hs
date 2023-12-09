import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "input.txt"
  let rows = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 rows)
  putStrLn $ "Part 2: " ++ (show $ part2 rows)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/09/" ++ fileName


part1 :: [[Int]] -> Int
part1 rows =
  sum nextNumbers
  where
    nextNumbers =
      map nextNumber finalDifferences

    finalDifferences =
      map (map last) $ map differences rows


part2 :: [[Int]] -> Int
part2 rows =
  sum prevNumbers
  where
    prevNumbers =
      map prevNumber startingDifferences

    startingDifferences =
      map (map head) $ map differences rows


-- -----------------------------------------------------------------------------
-- Helpers


differences :: [Int] -> [[Int]]
differences ns =
  loop [] ns
  where
    loop result row
      | all (== 0) row =
        row:result
      | otherwise =
        let
          nextRow =
            map (\(x,y) -> y - x) $ pairs row
        in
          loop (row:result) nextRow


nextNumber :: [Int] -> Int
nextNumber finalNumbers =
  foldl (+) 0 finalNumbers


prevNumber :: [Int] -> Int
prevNumber startingNumbers =
  foldl (flip (-)) 0 startingNumbers


pairs :: [a] -> [(a,a)]
pairs xs =
  case xs of
    [] ->
      []

    (_:rest) ->
      zip xs rest


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> [[Int]]
parseInput input =
  map parseRow $ lines input


parseRow :: String -> [Int]
parseRow line =
  map read $ words line
