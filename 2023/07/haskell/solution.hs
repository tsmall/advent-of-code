import Data.List (group, sort, sortBy)
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "../../../../advent-of-code-problems/2023/07/input.txt"
  putStrLn $ "Part 1: " ++ (show $ part1 input)
  putStrLn $ "Part 2: " ++ (show $ part2 input)


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


part1 :: String -> Int
part1 input =
  sum $ map (\(rank, bid) -> rank * bid) rankedBids
  where
    rankedBids =
      zip [1..] $ map (handBid . scoredHandHand) sortedHands

    sortedHands =
      sortHands scoredHands

    scoredHands =
      map score hands

    hands =
      parseInput input


part2 :: String -> Int
part2 input =
  sum $ map (\(rank, bid) -> rank * bid) rankedBids
  where
    rankedBids =
      zip [1..] $ map (handBid . scoredHandHand) sortedHands

    sortedHands =
      sortHands scoredHands

    scoredHands =
      map score hands

    hands =
      map makeJacksJokers $ parseInput input


-- -----------------------------------------------------------------------------
-- Data types


data Hand = Hand
  { handCards :: [Card]
  , handBid :: Int
  }
  deriving (Show)


data ScoredHand = ScoredHand
  { scoredHandHand :: Hand
  , scoredHandType :: HandType
  }
  deriving (Show)


data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord, Enum)


data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)


-- -----------------------------------------------------------------------------
-- Helpers


score :: Hand -> ScoredHand
score hand =
  ScoredHand hand bestHandType
  where
    bestHandType =
      maximum $ map handType cardPermutations

    cardPermutations =
      if any (== Joker) cards
      then
        replaceAllJokers cards
      else
        [cards]

    cards =
      handCards hand


replaceAllJokers :: [Card] -> [[Card]]
replaceAllJokers cards =
  map (replaceJokers cards) [Two .. Ace]


replaceJokers :: [Card] -> Card -> [Card]
replaceJokers cards replacement =
  map swapJoker cards
  where
    swapJoker card =
      if card == Joker
      then
        replacement
      else
        card


makeJacksJokers :: Hand -> Hand
makeJacksJokers hand =
  hand { handCards = newCards }
  where
    newCards =
      map swapJoker $ handCards hand

    swapJoker card =
      if card == Jack
      then
        Joker
      else
        card


handType :: [Card] -> HandType
handType cards =
  case countOccurrences cards of
    [5] -> FiveOfAKind
    [1,4] -> FourOfAKind
    [2,3] -> FullHouse
    [1,1,3] -> ThreeOfAKind
    [1,2,2] -> TwoPair
    [1,1,1,2] -> OnePair
    [1,1,1,1,1] -> HighCard


countOccurrences :: (Eq a, Ord a) => [a] -> [Int]
countOccurrences xs =
  sort $ map length $ group $ sort xs


sortHands :: [ScoredHand] -> [ScoredHand]
sortHands hands =
  sortBy compareHands hands


compareHands :: ScoredHand -> ScoredHand -> Ordering
compareHands (ScoredHand hand1 handType1) (ScoredHand hand2 handType2) =
  case compare handType1 handType2 of
    LT ->
      LT

    GT ->
      GT

    EQ ->
      compare (handCards hand1) (handCards hand2)


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> [Hand]
parseInput input =
  map parseLine $ lines input


parseLine :: String -> Hand
parseLine line =
  Hand cards bid
  where
    cards =
      map parseCard $ word1

    bid =
      read word2

    [word1, word2] =
      words line


parseCard :: Char -> Card
parseCard card =
  case card of
    '2' -> Two
    '3' -> Three
    '4' -> Four
    '5' -> Five
    '6' -> Six
    '7' -> Seven
    '8' -> Eight
    '9' -> Nine
    'T' -> Ten
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace
