import qualified Data.Map.Strict as Map
import System.IO (IOMode(..), openFile, hGetContents)
import Text.Parsec (ParseError, choice, many1, parse, sepBy)
import Text.Parsec.Char (char, digit, letter, newline, spaces, string)
import Text.Parsec.String (Parser)


main :: IO ()
main = do
  input <- load "../../../../advent-of-code-problems/2023/02/input.txt"
  case parseInput input of
    Left error ->
      print error

    Right games -> do
      let part1Answer = part1 games
      let part2Answer = part2 games
      putStrLn $ "Part 1: " ++ (show part1Answer)
      putStrLn $ "Part 2: " ++ (show part2Answer)


load :: String -> IO String
load filePath =
  openFile filePath ReadMode
  >>= hGetContents


data Game = Game
  { gameID :: Int
  , gameCubes :: [Cube]
  }
  deriving (Show)


data Cube = Cube
  { cubeCount :: Int
  , cubeColor :: String
  }
  deriving (Show)


part1 :: [Game] -> Int
part1 games =
  sum validGameIDs
  where
    validGameIDs =
      map gameID validGames

    validGames =
      filter isValidGame games


isValidGame :: Game -> Bool
isValidGame game =
  all isValidCube (gameCubes game)


isValidCube :: Cube -> Bool
isValidCube (Cube count color) =
  case color of
    "red" ->
      count <= 12

    "green" ->
      count <= 13

    "blue" ->
      count <= 14


part2 :: [Game] -> Int
part2 games =
  sum powers
  where
    powers =
      map power minCubes

    minCubes =
      map minPossibleCubes games


power :: [Cube] -> Int
power cubes =
  product counts
  where
    counts =
      map cubeCount cubes


minPossibleCubes :: Game -> [Cube]
minPossibleCubes game =
  map toCube (Map.assocs finalMap)
  where
    toCube (color, count) =
      Cube count color

    finalMap =
      foldl updateMap Map.empty cubes

    updateMap map cube =
      Map.insertWith max (cubeColor cube) (cubeCount cube) map

    cubes =
      gameCubes game


-- -----------------------------------------------------------------------------
-- Parsing


parseInput :: String -> Either ParseError [Game]
parseInput =
  parse (many1 parseGame) ""


parseGame :: Parser Game
parseGame = do
  gameID <- parseGameID
  cubes <- parseCubes
  spaces
  return Game
    { gameID = gameID
    , gameCubes = cubes
    }


parseGameID :: Parser Int
parseGameID =
  read <$> (string "Game " *> many1 digit <* char ':')


parseCubes :: Parser [Cube]
parseCubes =
  parseCube `sepBy` (choice [char ';', char ','] <* spaces)


parseCube :: Parser Cube
parseCube = do
  spaces
  count <- read <$> many1 digit
  spaces
  color <- many1 letter
  return $ Cube count color
