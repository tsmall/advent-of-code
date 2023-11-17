import System.IO

main :: IO ()
main = do
  handle <- openFile "../input.txt" ReadMode
  input <- hGetContents handle
  let part1 = finalFloor 0 input
  let part2 = stepsToBasement 0 0 input
  putStrLn $ "Part 1: " ++ (show part1)
  putStrLn $ "Part 2: " ++ (show part2)

finalFloor :: Int -> String -> Int
finalFloor floor instructions =
  case instructions of
    "" ->
      floor
    (instruction:rest) ->
      finalFloor newFloor rest
      where
        newFloor = move floor instruction

stepsToBasement :: Int -> Int -> String -> Int
stepsToBasement floor steps (instruction:rest) =
  if
    floor < 0
  then
    steps
  else
    stepsToBasement newFloor newSteps rest
    where
      newFloor = move floor instruction
      newSteps = succ steps

move :: Int -> Char -> Int
move floor instruction =
  case instruction of
    '(' -> succ floor
    ')' -> pred floor
    _   -> floor
