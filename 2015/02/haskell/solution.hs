import Data.List (sort)
import System.IO (IOMode(..), openFile, hGetContents)


main :: IO ()
main = do
  input <- input
  let boxes = map parseBox (lines input)
  let paper = map paperNeeded boxes
  let ribbon = map ribbonNeeded boxes
  putStrLn $ "Part 1: " ++ (show $ sum paper)
  putStrLn $ "Part 2: " ++ (show $ sum ribbon)


input :: IO String
input =
  openFile "../input.txt" ReadMode
  >>= hGetContents


data Box = Box
  { boxLength :: Int
  , boxWidth :: Int
  , boxHeight :: Int
  }
  deriving Show


paperNeeded :: Box -> Int
paperNeeded box =
  2*l*w + 2*w*h + 2*h*l + smallestArea
  where
    l = boxLength box
    w = boxWidth box
    h = boxHeight box
    smallestArea = minimum [l*w, w*h, h*l]


ribbonNeeded :: Box -> Int
ribbonNeeded box =
  smallestPerimeter + cubicVolume
  where
    smallestPerimeter = sum $ take 2 $ sort perimeters
    cubicVolume = product sides
    perimeters = map (2*) sides
    sides = [boxLength box, boxWidth box, boxHeight box]


parseBox :: String -> Box
parseBox string =
  Box l w h
  where
    [l, w, h] = map read words
    words = wordsBy (== 'x') string


wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy pred string =
  case dropWhile pred string of
    "" ->
      []
    s ->
      w : wordsBy pred rest
      where
        (w, rest) = break pred s
