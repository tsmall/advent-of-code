import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Data.Map.Strict (Map, (!?))
import Data.Sequence (Seq, (|>), (><))
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main =
  do
    input <- load "input.txt"
    let line = head $ lines input
    let steps = splitOn (== ',') line
    putStrLn $ "Part 1: " ++ (show $ part1 steps)
    putStrLn $ "Part 2: " ++ (show $ part2 steps)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/15/" ++ fileName


part1 :: [String] -> Int
part1 steps =
  sum $ map hash steps


part2 :: [String] -> Int
part2 steps =
  focusPower $ runSteps Map.empty steps


-- -----------------------------------------------------------------------------
-- Data types


type Boxes =
  Map Int (Seq Lens)


data Lens = Lens
  { label :: String
  , focalLength :: Int
  }
  deriving (Show)


data Step = Step
  { stepLabel :: String
  , stepOperation :: StepOperation
  }
  deriving (Show)


data StepOperation
  = Remove
  | Put Int
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


hash :: String -> Int
hash string =
  doHash 0 string
  where
    doHash value [] =
      value

    doHash value (c:rest) =
      doHash (((value + (Char.ord c)) * 17) `mod` 256) rest


runSteps :: Boxes -> [String] -> Boxes
runSteps boxes steps =
  foldl processStep boxes steps


processStep :: Boxes -> String -> Boxes
processStep boxes step =
  case operation of
    Remove ->
      removeLens boxNum label boxes

    Put focalLength ->
      putLens boxNum (Lens label focalLength) boxes

  where
    (Step label operation) =
      parseStep step

    boxNum =
      hash label


removeLens :: Int -> String -> Boxes -> Boxes
removeLens boxNum lensLabel boxes =
  case boxes !? boxNum of
    Nothing ->
      boxes

    Just lenses ->
      Map.insert boxNum newLenses boxes
      where
        newLenses =
          removeLens' lensLabel lenses


removeLens' :: String -> Seq Lens -> Seq Lens
removeLens' lensLabel lenses =
  case Seq.findIndexL (hasLabel lensLabel) lenses of
    Nothing ->
      lenses

    Just i ->
      before >< after
      where
        before =
          Seq.take i lenses

        after =
          Seq.drop (i + 1) lenses


putLens :: Int -> Lens -> Boxes -> Boxes
putLens boxNum lens boxes =
  Map.insert boxNum newLenses boxes
  where
    newLenses =
      case boxes !? boxNum of
        Nothing ->
          Seq.singleton lens

        Just lenses ->
          putLens' lens lenses


putLens' :: Lens -> Seq Lens -> Seq Lens
putLens' lens lenses =
  case Seq.findIndexL (hasLabel $ label lens) lenses of
    Nothing ->
      lenses |> lens

    Just i ->
      Seq.update i lens lenses


hasLabel :: String -> Lens -> Bool
hasLabel lensLabel lens =
  lensLabel == (label lens)


focusPower :: Boxes -> Int
focusPower boxes =
  sum $ Map.elems $ Map.mapWithKey boxPower boxes


boxPower :: Int -> Seq Lens -> Int
boxPower boxNum lenses =
  sum $ Seq.mapWithIndex (lensPower boxNum) lenses


lensPower :: Int -> Int -> Lens -> Int
lensPower boxNum slotNum lens =
  (1 + boxNum) * (1 + slotNum) * (focalLength lens)


-- -----------------------------------------------------------------------------
-- Parsing


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred xs =
  case dropWhile pred xs of
    [] ->
      []

    xs' ->
      h : splitOn pred t
      where
        (h, t) =
          break pred xs'


parseStep :: String -> Step
parseStep step =
  Step label operation
  where
    label =
      takeWhile Char.isAlpha step

    operation =
      parseOperation $ dropWhile Char.isAlpha step


parseOperation :: String -> StepOperation
parseOperation operation =
  case operation of
    "-" ->
      Remove

    ('=':numberString) ->
      Put $ read numberString
