module Day12 where

import Data.List (findIndex, foldl')
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Common (textToInt, readFileUtf8)

part1 :: IO ()
part1 = do
  insts <- parseInstructions <$> readFileUtf8 "input/day_12.txt"
  let (_, x, y) = foldl' (flip runInstruction) ('E', 0, 0) insts
  print ((abs x) + (abs y))

part2 :: IO ()
part2 = do
  insts <- parseInstructions <$> readFileUtf8 "input/day_12.txt"
  let ((x, y), _) = foldl' runInstructionWP ((0, 0), (10, 1)) insts
  print ((abs x) + (abs y))

rotate :: Char -> Int -> Char
rotate dir angle = dirs !! ((start + (angle `div` 90)) `mod` 4)
  where dirs  = ['N', 'E', 'S', 'W']
        start = fromJust $ findIndex (==dir) dirs

rotateAroundOrigin :: (Int, Int) -> Int -> (Int, Int)
rotateAroundOrigin (x, y) angle = rotate90 (x, y) (angle `div` 90)
  where rotate90 (x, y) 0 = (x, y)
        rotate90 (x, y) n = rotate90 (y, -x) (n-1)

runInstructionWP :: ((Int, Int), (Int, Int)) -> (Char, Int) -> ((Int, Int), (Int, Int))
runInstructionWP p                    ('L', n) = runInstructionWP p ('R', 360 - n)
runInstructionWP (s,        (wx, wy)) ('R', n) = (s, rotateAroundOrigin (wx, wy) n)
runInstructionWP ((sx, sy), (wx, wy)) ('F', n) = ((sx + n * wx, sy + n * wy), (wx, wy))
runInstructionWP (s,        (wx, wy)) inst     = (s, wpos')
  where wpos'         = (wx', wy')
        (_, wx', wy') = runInstruction inst ('X', wx, wy)

runInstruction :: (Char, Int) -> (Char, Int, Int) -> (Char, Int, Int)
runInstruction ('F', n)   pos@(dir, _, _)  = runInstruction (dir, n) pos
runInstruction ('N', n)   (dir, x, y)      = (dir, x, y + n)
runInstruction ('E', n)   (dir, x, y)      = (dir, x + n, y)
runInstruction ('W', n)   (dir, x, y)      = (dir, x - n, y)
runInstruction ('S', n)   (dir, x, y)      = (dir, x, y - n)
runInstruction ('L', n)   pos              = runInstruction ('R', 360 - n) pos
runInstruction ('R', n)   (dir, x, y)      = (rotate dir n, x, y)

parseInstructions :: Text -> [(Char, Int)]
parseInstructions = fmap parseInstruction . Text.lines
  where parseInstruction t = (Text.head t, textToInt (Text.drop 1 t))
