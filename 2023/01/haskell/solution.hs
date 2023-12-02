import Data.Char (isDigit)
import Data.List (isPrefixOf)
import System.IO (IOMode(..), openFile, hGetContents)


main :: IO ()
main = do
  input <- load "../input.txt"
  let inputLines = lines input
  putStrLn $ "Part 1: " ++ (show $ part1 inputLines)
  putStrLn $ "Part 2: " ++ (show $ part2 inputLines)


part1 :: [String] -> Int
part1 inputLines =
  sum values
  where
    values = map numberFromString inputLines


part2 :: [String] -> Int
part2 inputLines =
  sum values
  where
    values = map numberFromString fixedLines
    fixedLines = map unspellNumbers inputLines


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


numberFromString :: String -> Int
numberFromString s =
  read $ digitsInString s


digitsInString :: String -> String
digitsInString s =
  [firstDigit, lastDigit]
  where
    firstDigit = head digits
    lastDigit = last digits
    digits = filter isDigit s


unspellNumbers :: String -> String
unspellNumbers s =
  unspell "" s
  where
    unspell result rest =
      case rest of
        [] ->
          result

        s' ->
          case spelledNumber s' of
            Found number ->
              let
                rest' = tail s'
                result' = result ++ number
              in
                unspell result' rest'

            NotFound ->
              let
                rest' = tail s'
                result' = result ++ (take 1 s')
              in
                unspell result' rest'


data SearchResult
  = Found String
  | NotFound

spelledNumber :: String -> SearchResult
spelledNumber s =
  if "one" `isPrefixOf` s
  then
    Found "1"
  else if "two" `isPrefixOf` s
  then
    Found "2"
  else if "three" `isPrefixOf` s
  then
    Found "3"
  else if "four" `isPrefixOf` s
  then
    Found "4"
  else if "five" `isPrefixOf` s
  then
    Found "5"
  else if "six" `isPrefixOf` s
  then
    Found "6"
  else if "seven" `isPrefixOf` s
  then
    Found "7"
  else if "eight" `isPrefixOf` s
  then
    Found "8"
  else if "nine" `isPrefixOf` s
  then
    Found "9"
  else
    NotFound

