import qualified Data.Map.Strict as Map

import Data.Char (isAlpha, isSpace)
import Data.List (foldl1, isSuffixOf)
import Data.Map.Strict (Map, (!))
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "input.txt"
  let (network, instructions) = parseInput input
  putStrLn $ "Part 1: " ++ (show $ part1 network instructions)
  putStrLn $ "Part 2: " ++ (show $ part2 network instructions)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/08/" ++ fileName


part1 :: Network -> [Instruction] -> Int
part1 network instructions =
  navigate "AAA" network instructions (== "ZZZ")


part2 :: Network -> [Instruction] -> Int
part2 network instructions =
  foldl1 lcm stepsPerNode
  where
    stepsPerNode =
      map doNav startNodes

    doNav node =
      navigate node network instructions ("Z" `isSuffixOf`)

    startNodes =
      filter ("A" `isSuffixOf`) $ Map.keys network


-- -----------------------------------------------------------------------------
-- Data types


type Network =
  Map String (String, String)


data Instruction
  = GoLeft
  | GoRight
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


navigate :: String -> Network -> [Instruction] -> (String -> Bool) -> Int
navigate startNode network instructions isEnd =
  loop 1 startNode (cycle instructions)
  where
    loop count currentNode (instruction:rest) =
      let
        options = network ! currentNode
        nextNode = choose instruction options
      in
        if isEnd nextNode
        then
          count
        else
          loop (count + 1) nextNode rest


choose :: Instruction -> (String, String) -> String
choose GoLeft (l,r) = l
choose GoRight (l,r) = r


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> (Network, [Instruction])
parseInput input =
  (network, instructions)
  where
    instructions =
      parseInstructions firstLine

    network =
      parseNetworks $ drop 2 inputLines

    firstLine =
      head inputLines

    inputLines =
      lines input


parseInstructions :: String -> [Instruction]
parseInstructions line =
  map parseInstruction line


parseInstruction :: Char -> Instruction
parseInstruction 'L' = GoLeft
parseInstruction 'R' = GoRight


parseNetworks :: [String] -> Network
parseNetworks networkLines =
  Map.fromList $ map parseNetworkLine networkLines


parseNetworkLine :: String -> (String, (String, String))
parseNetworkLine line =
  (word1, (word2, word3))
  where
    [word1, word2, word3] =
      words filteredLine

    filteredLine =
      filter (\c -> (isAlpha c) || (isSpace c)) line
