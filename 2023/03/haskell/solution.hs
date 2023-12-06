import Data.Char (isNumber)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main = do
  input <- load "../../../../advent-of-code-problems/2023/03/input.txt"
  let grid = parseGrid input
  let numbers = parseNumbers grid
  putStrLn $ "Part 1: " ++ (show $ part1 grid numbers)
  putStrLn $ "Part 2: " ++ (show $ part2 grid numbers)


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


part1 :: Grid -> [Number] -> Int
part1 grid numbers =
  sum $ map numberValue $ validNumbers grid numbers


part2 :: Grid -> [Number] -> Int
part2 grid numbers =
  sum gearRatios
  where
    gearRatios =
      map product numberValues

    numberValues =
      map (map numberValue) validGearNumbers

    validGearNumbers =
      filter ((== 2) . length) gearNumbers

    gearNumbers =
      map (adjacentNumbers grid numbers) (gearPoints grid)


-- -----------------------------------------------------------------------------
-- Data types


data Number = Number
  { numberStart :: Point
  , numberEnd :: Point
  , numberValue :: Int
  }
  deriving (Show, Eq, Ord)


data Point = Point
  { pointX :: Int
  , pointY :: Int
  }
  deriving (Show, Eq, Ord)


data Grid = Grid
  { gridMin :: Point
  , gridMax :: Point
  , gridLines :: [String]
  }
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


validNumbers :: Grid -> [Number] -> [Number]
validNumbers grid numbers =
  filter (isNextToSymbol grid) numbers


isNextToSymbol :: Grid -> Number -> Bool
isNextToSymbol grid number =
  any isSymbol adjacentValues
  where
    adjacentValues =
      Set.map (cellValue grid) adjacentPoints

    adjacentPoints =
      pointsAroundNumber grid number


isSymbol :: Char -> Bool
isSymbol char =
  not ((isNumber char) || (char == '.'))


adjacentNumbers :: Grid -> [Number] -> Point -> [Number]
adjacentNumbers grid numbers point =
  filter (isAdjacent grid point) numbers


isAdjacent :: Grid -> Point -> Number -> Bool
isAdjacent grid point number =
  not $ Set.disjoint numberPoints adjacentPoints
  where
    numberPoints =
      Set.fromList $ pointsInNumber number

    adjacentPoints =
      pointsAroundPoint grid point


gearPoints :: Grid -> [Point]
gearPoints grid =
  loop [] 0 (gridLines grid)
  where
    loop points _ [] =
      points
    loop points y (line:rest) =
      loop (points ++ (gearPointsInLine line y)) (y + 1) rest


gearPointsInLine :: String -> Int -> [Point]
gearPointsInLine line y =
  loop [] 0 line
  where
    loop points _ [] =
      points
    loop points x (char:rest)
      | isGear char =
        loop ((Point x y) : points) (x + 1) rest
      | otherwise =
        loop points (x + 1) rest


isGear :: Char -> Bool
isGear =
  (== '*')


cellValue :: Grid -> Point -> Char
cellValue (Grid _ _ lines) (Point x y) =
  lines !! y !! x


pointsAroundNumber :: Grid -> Number -> Set Point
pointsAroundNumber grid number =
  foldl addSurroundingPoints Set.empty numberPoints
  where
    addSurroundingPoints allPoints point =
      Set.union (pointsAround point) allPoints

    pointsAround =
      pointsAroundPoint grid

    numberPoints =
      pointsInNumber number


pointsAroundPoint :: Grid -> Point -> Set Point
pointsAroundPoint (Grid (Point minX minY) (Point maxX maxY) _) (Point x y) =
  Set.fromList
    [ Point (x + dx) (y + dy)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , x + dx >= minX
    , x + dx <= maxX
    , y + dy >= minY
    , y + dy <= maxY
    ]


pointsInNumber :: Number -> [Point]
pointsInNumber number =
  [ Point x y | x <- [startX..endX] ]
  where
    y =
      pointY $ numberStart number

    startX =
      pointX $ numberStart number

    endX =
      pointX $ numberEnd number


-- -----------------------------------------------------------------------------
-- Parsing


parseGrid :: String -> Grid
parseGrid input =
  let
    inputLines =
      lines input

    maxY =
      (length inputLines) - 1

    maxX =
      (length $ inputLines !! 0) - 1
  in
    Grid
      { gridMin = Point { pointX = 0, pointY = 0 }
      , gridMax = Point { pointX = maxX, pointY = maxY }
      , gridLines = inputLines
      }


parseNumbers :: Grid -> [Number]
parseNumbers (Grid _ _ lines) =
  fst $ foldl addNumbersInLine ([], 0) lines
  where
    addNumbersInLine (numbers, lineNumber) line =
      let
        newNumbers =
          numbers ++ (parseNumbersInLine line lineNumber)

        newLineNumber =
          lineNumber + 1
      in
        (newNumbers, newLineNumber)


parseNumbersInLine :: String -> Int -> [Number]
parseNumbersInLine line y =
  loop [] "" 0 0 line
  where
    -- loop numbers current startX currentX rest
    loop numbers "" _ _ [] =
      numbers
    loop numbers current startX currentX [] =
      (newNumber current startX currentX) : numbers
    loop numbers "" startX currentX (char:rest)
      | isNumber char =
        loop numbers [char] currentX (currentX + 1) rest
      | otherwise =
        loop numbers "" startX (currentX + 1) rest
    loop numbers current startX currentX (char:rest)
      | isNumber char =
        loop numbers (current ++ [char]) startX (currentX + 1) rest
      | otherwise =
        loop ((newNumber current startX currentX) : numbers) "" startX (currentX + 1) rest

    newNumber string startX currentX =
      Number
        { numberStart = Point { pointX = startX, pointY = y }
        , numberEnd = Point { pointX = currentX - 1, pointY = y }
        , numberValue = read string
        }
