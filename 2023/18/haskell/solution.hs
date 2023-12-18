import Numeric (readHex)
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main =
  do
    input <- load "input.txt"
    let (part1Instructions, part2Instructions) = parseInstructions input
    putStrLn $ "Part 1: " ++ (show $ area part1Instructions)
    putStrLn $ "Part 2: " ++ (show $ area part2Instructions)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/18/" ++ fileName


-- -----------------------------------------------------------------------------
-- Data types


data Point = Point
  { pointX :: Int
  , pointY :: Int
  }


instance Show Point where
  show (Point x y) =
    concat ["(", show x, ",", show y, ")"]


data Instruction = Instruction
  { instDirection :: Direction
  , instDistance :: Int
  }
  deriving (Show)


data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


area :: [Instruction] -> Int
area instructions =
  areaLoop 0 0 (Point 0 0) instructions


areaLoop :: Int -> Int -> Point -> [Instruction] -> Int
areaLoop area perimeter point instructions =
  case instructions of
    [] ->
      area `div` 2 + perimeter `div` 2 + 1

    (instruction : rest) ->
      let
        newPerimeter =
          perimeter + (instDistance instruction)

        nextPoint =
          movePoint instruction point

        newArea =
          area + (nextX + currentX) * (nextY - currentY)
          where
            (Point nextX nextY) =
              nextPoint

            (Point currentX currentY) =
              point

      in
        areaLoop newArea newPerimeter nextPoint rest


movePoint :: Instruction -> Point -> Point
movePoint (Instruction direction distance) point =
  case direction of
    DirUp ->
      point { pointY = pointY point - distance }

    DirDown ->
      point { pointY = pointY point + distance }

    DirLeft ->
      point { pointX = pointX point - distance }

    DirRight ->
      point { pointX = pointX point + distance }


-- -----------------------------------------------------------------------------
-- Parsing


parseInstructions :: String -> ([Instruction], [Instruction])
parseInstructions input =
  unzip $ map parse $ lines input
  where
    parse line =
      ( parseInstruction direction distance
      , parseColor color
      )
      where
        [direction, distance, color] =
          words line


parseInstruction :: String -> String -> Instruction
parseInstruction direction distance =
  Instruction
    { instDirection = parseDirection direction
    , instDistance = read distance
    }


parseColor :: String -> Instruction
parseColor s =
  Instruction
    { instDirection = parseDirection direction
    , instDistance = distance
    }
  where
    direction =
      drop 5 hexDigits

    [(distance, "")] =
      readHex $ take 5 hexDigits

    hexDigits =
      take 6 $ drop 2 s


parseDirection :: String -> Direction
parseDirection s =
  case s of
    "U" ->
      DirUp

    "3" ->
      DirUp

    "D" ->
      DirDown

    "1" ->
      DirDown

    "L" ->
      DirLeft

    "2" ->
      DirLeft

    "R" ->
      DirRight

    "0" ->
      DirRight
