import qualified Data.Set as Set

import Data.Set (Set)
import System.IO (IOMode(..), openFile, hGetContents)
import Text.Parsec (ParseError, (<|>), choice, many1, parse, sepBy)
import Text.Parsec.Char (char, digit, newline, spaces, string)
import Text.Parsec.String (Parser)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "../input.txt"
  case parseInput input of
    Left error ->
      print error

    Right cards -> do
      putStrLn $ "Part 1: " ++ (show $ part1 cards)
      putStrLn $ "Part 2: " ++ (show $ part2 cards)


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


part1 :: [Card] -> Int
part1 cards =
  sum cardWorths
  where
    cardWorths =
      map cardWorth winningNumberCounts

    winningNumberCounts =
      map (length . winningNumbers) cards


part2 :: [Card] -> Int
part2 cards =
  sum cardCounts
  where
    cardCounts =
      map instanceCount finalInstances

    finalInstances =
      foldl (flip copyBasedOnWins) initialInstances indexes

    initialInstances =
      map (\card -> CardInstances card 1) cards

    indexes =
      [0 .. (length initialInstances) - 1]


-- -----------------------------------------------------------------------------
-- Data types


data Card = Card
  { cardID :: Int
  , cardWinningNumbers :: Set Int
  , cardNumbers :: Set Int
  }
  deriving (Show)


data CardInstances = CardInstances
  { instanceCard :: Card
  , instanceCount :: Int
  }
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


cardWorth :: Int -> Int
cardWorth 0 =
  0
cardWorth winningNumberCount =
  2 ^ (winningNumberCount - 1)


winningNumbers :: Card -> Set Int
winningNumbers card =
  Set.intersection (cardNumbers card) (cardWinningNumbers card)


copyBasedOnWins :: Int -> [CardInstances] -> [CardInstances]
copyBasedOnWins index instances =
  foldl increment instances [startIndex..endIndex]
  where
    increment instances index =
      incrementInstances count index instances

    count =
      instanceCount $ instances !! index

    startIndex =
      index + 1

    endIndex =
      (index + winningNumberCount) `min` (length instances)

    winningNumberCount =
      length $ winningNumbers $ instanceCard $ instances !! index


incrementInstances :: Int -> Int -> [CardInstances] -> [CardInstances]
incrementInstances count index instances =
  updateElement incrementCount index instances
  where
    incrementCount ci =
      ci { instanceCount = (instanceCount ci) + count }


updateElement :: (a -> a) -> Int -> [a] -> [a]
updateElement f index xs =
  beforeElements ++ [updatedElement] ++ afterElements
  where
    beforeElements =
      take index xs

    updatedElement =
      f $ xs !! index

    afterElements =
      drop (index + 1) xs


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> Either ParseError [Card]
parseInput =
  parse (many1 parseCard) ""


parseCard :: Parser Card
parseCard = do
  cardID <- parseCardID
  spaces
  winningNumbers <- parseNumbers
  char '|'
  spaces
  numbers <- parseNumbers
  return $ Card cardID winningNumbers numbers


parseCardID :: Parser Int
parseCardID =
  read <$> (string "Card" *> spaces *> many1 digit <* char ':')


parseNumbers :: Parser (Set Int)
parseNumbers =
  Set.fromList <$> many1 (parseNumber <* spaces)


parseNumber :: Parser Int
parseNumber =
  read <$> many1 digit
