import qualified Data.Map.Strict as Map

import Data.Char (isAlpha, isSpace)
import Data.Map.Strict (Map, (!))
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  -- input <- load "example2.txt"
  input <- load "input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 input)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/08/" ++ fileName


part1 :: String -> Int
part1 input =
  navigate network instructions
  where
    (network, instructions) =
      parseInput input


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


navigate :: Network -> [Instruction] -> Int
navigate network instructions =
  loop 1 "AAA" (cycle instructions)
  where
    loop count currentNode (instruction:rest) =
      let
        options = network ! currentNode
        nextNode = choose instruction options
      in
        if nextNode == "ZZZ"
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
  foldl readInstruction [] line
  where
    readInstruction instructions char =
      instructions ++ [parseInstruction char]


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
