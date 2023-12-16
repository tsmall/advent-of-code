import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map, (!?))
import Data.Set (Set)
import System.IO (IOMode(..), openFile, hGetContents)


-- -----------------------------------------------------------------------------
-- Main


main :: IO ()
main =
  do
    input <- load "input.txt"
    let tileMap = parseMap input
    putStrLn $ "Part 1: " ++ (show $ part1 tileMap)
    putStrLn $ "Part 2: " ++ "TODO" -- (show $ part2 input)


load :: String -> IO String
load fileName =
  openFile filePath ReadMode >>= hGetContents
  where
    filePath =
      "../../../../advent-of-code-problems/2023/16/" ++ fileName


part1 :: TileMap -> Int
part1 tileMap =
  shootPhotons tileMap Set.empty [Photon (Point 0 0) East]


-- -----------------------------------------------------------------------------
-- Data types


data Photon = Photon
  { photonPoint :: Point
  , photonDirection :: Direction
  }
  deriving (Show, Eq, Ord)


data Point = Point
  { pointX :: Int
  , pointY :: Int
  }
  deriving (Show, Eq, Ord)


data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)


data TileMap = TileMap
  { mapTiles :: Map Point Tile
  , mapMinX :: Int
  , mapMaxX :: Int
  , mapMinY :: Int
  , mapMaxY :: Int
  }
  deriving (Show)


data Tile
  = Empty
  | SplitterVertical
  | SplitterHorizontal
  | MirrorRight
  | MirrorLeft
  deriving (Show)


-- -----------------------------------------------------------------------------
-- Helpers


shootPhotons :: TileMap -> Set Photon -> [Photon] -> Int
shootPhotons tileMap history photons =
  case photons of
    [] ->
      length $ Set.map photonPoint history

    (photon : rest) ->
      if Set.member photon history
      then
        -- Skip photons we have already seen.
        shootPhotons tileMap history rest
      else
        -- Otherwise record that we've seen it
        -- and queue up the next photons.
        shootPhotons tileMap newHistory (newPhotons ++ photons)

      where
        newHistory =
          Set.insert photon history

        newPhotons =
          filter (isInBounds tileMap) $ map move $ bounce tile photon

        tile =
          (mapTiles tileMap) !? (photonPoint photon)


isInBounds :: TileMap -> Photon -> Bool
isInBounds tileMap photon =
  let
    Photon (Point x y) _ =
      photon

    TileMap _ minX maxX minY maxY =
      tileMap

  in
    (x >= minX) && (x <= maxX) && (y >= minY) && (y <= maxY)


move :: Photon -> Photon
move (Photon point direction) =
  Photon newPoint direction
  where
    newPoint =
      case direction of
        North ->
          point { pointY = originalY - 1 }

        East ->
          point { pointX = originalX + 1 }

        South ->
          point { pointY = originalY + 1 }

        West ->
          point { pointX = originalX - 1 }

    (Point originalX originalY) =
      point


bounce :: Maybe Tile -> Photon -> [Photon]
bounce maybeTile photon =
  let
    (Photon _ direction) =
      photon

  in
    case maybeTile of
      Nothing ->
        []

      Just Empty ->
        [ photon ]

      Just SplitterVertical
        | direction == East || direction == West ->
          [ photon { photonDirection = North }
          , photon { photonDirection = South }
          ]
        | otherwise ->
          [ photon ]

      Just SplitterHorizontal
        | direction == North || direction == South ->
          [ photon { photonDirection = East }
          , photon { photonDirection = West }
          ]
        | otherwise ->
          [ photon ]

      Just MirrorRight
        | direction == North ->
          [ photon { photonDirection = East } ]
        | direction == East ->
          [ photon { photonDirection = North } ]
        | direction == South ->
          [ photon { photonDirection = West } ]
        | direction == West ->
          [ photon { photonDirection = South } ]

      Just MirrorLeft
        | direction == North ->
          [ photon { photonDirection = West } ]
        | direction == East ->
          [ photon { photonDirection = South } ]
        | direction == South ->
          [ photon { photonDirection = East } ]
        | direction == West ->
          [ photon { photonDirection = North } ]


-- -----------------------------------------------------------------------------
-- Parsing


parseMap :: String -> TileMap
parseMap input =
  TileMap
    { mapTiles = tiles
    , mapMinX = minimum xs
    , mapMaxX = maximum xs
    , mapMinY = minimum ys
    , mapMaxY = maximum ys
    }
  where
    xs =
      map pointX $ Map.keys tiles

    ys =
      map pointY $ Map.keys tiles

    tiles =
      Map.fromList $ concatMap parse $ zip [0..] (lines input)

    parse (y, line) =
      parseRow y line


parseRow :: Int -> String -> [(Point, Tile)]
parseRow y line =
  map parse (zip [0..] line)
  where
    parse (x, char) =
      (Point x y, parseTile char)


parseTile :: Char -> Tile
parseTile char =
  case char of
    '.' ->
      Empty

    '|' ->
      SplitterVertical

    '-' ->
      SplitterHorizontal

    '/' ->
      MirrorRight

    '\\' ->
      MirrorLeft
