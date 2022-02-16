import Data.Char (isSpace)
import Data.Set (empty, insert, member)


main :: IO ()
main = do
  input <- readFile "../input.txt"
  putStrLn $ "Part 1: " <> show (part1 input)
  putStrLn $ "Part 2: " <> show (part2 input)


part1 :: String -> Int
part1 input =
  manhattanDistance finalPoint
  where
    finalPoint =
      last $ allPoints startPosition commands

    commands =
      parse input


part2 :: String -> Int
part2 input =
  manhattanDistance firstRepeatedPoint
  where
    firstRepeatedPoint =
      firstDuplicate $ allPoints startPosition commands

    commands =
      parse input


data Position =
  Position
    { point :: Point
    , heading :: Heading
    }
  deriving (Show)


type Point =
  (Int, Int)


data Heading
  = N
  | E
  | S
  | W
  deriving (Show)


data Turn
  = L
  | R
  deriving (Show)


data Command =
  Command Turn Int
  deriving (Show)


manhattanDistance :: Point -> Int
manhattanDistance (x,y) =
  abs x + abs y


startPosition :: Position
startPosition =
  Position { point = (0,0), heading = N }


allPoints :: Position -> [Command] -> [Point]
allPoints position commands =
  concatMap snd allResults
  where
    allResults =
      scanl (moveAll . fst) (position, []) commands


moveAll :: Position -> Command -> (Position, [Point])
moveAll position (Command turn distance) =
  let
    newHeading =
      rotate (heading position) turn

    newPoints =
      drop 1 $ walk (point position) newHeading distance

    newPosition =
      position { heading = newHeading, point = last newPoints }
  in
    (newPosition, newPoints)


firstDuplicate :: [Point] -> Point
firstDuplicate points =
  loop empty points
  where
    loop seen [] =
      error "No duplicate found!"
    loop seen (p:ps) =
      if p `member` seen
      then p
      else loop (insert p seen) ps


rotate :: Heading -> Turn -> Heading
rotate N L = W
rotate N R = E
rotate E L = N
rotate E R = S
rotate S L = E
rotate S R = W
rotate W L = S
rotate W R = N


walk :: Point -> Heading -> Int -> [Point]
walk point heading distance =
  take (distance + 1) $ iterate (step heading) point


step :: Heading -> Point -> Point
step N (x,y) = (x  , y+1)
step E (x,y) = (x+1, y  )
step S (x,y) = (x  , y-1)
step W (x,y) = (x-1, y  )


parse :: String -> [Command]
parse input =
  map (parseCommand . trim) $ split ',' input


parseCommand :: String -> Command
parseCommand word =
  case word of
    'L':d ->
      Command L (read d)
    'R':d ->
      Command R (read d)
    _ ->
      error $ "Invalid command: " <> word


split :: Char -> String -> [String]
split c s =
  case rest of
    []     -> [chunk]
    _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s


trim :: String -> String
trim =
  dropWhile isSpace
