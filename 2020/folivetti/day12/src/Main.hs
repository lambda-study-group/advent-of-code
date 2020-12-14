module Main where

-- | has this function type gone too far?
solve :: (Char -> Int -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))) -> (Int, Int) -> [(Char, Int)] -> Int
solve f = go (0,0) 
  where
    go (x, y) _ [] = abs x + abs y
    go (x, y) (dx, dy) ((d,n):ds) = 
       case d of
            'F' -> go (x+n*dx, y+n*dy) (dx, dy) ds
            'L' -> go (x, y) (rotL n (dx,dy)) ds
            'R' -> go (x, y) (rotR n (dx, dy)) ds
            _   -> let (xy, dxy) = f d n (x,y) (dx, dy)
                   in  go xy dxy ds

rotL, rotR :: Int -> (Int, Int) -> (Int, Int)
rotL 90  (dx, dy) = (-dy, dx)
rotL 180 (dx, dy) = (-dx, -dy)
rotL 270 (dx, dy) = (dy, -dx)

rotR 180 d = rotL 180 d
rotR n   d = rotL ((n+180) `mod` 360) d

part1, part2 :: Char -> Int -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
part1 'N' n (x, y) (dx, dy) = ((x, y+n), (dx, dy))
part1 'S' n (x, y) (dx, dy) = ((x, y-n), (dx, dy))
part1 'E' n (x, y) (dx, dy) = ((x+n, y), (dx, dy))
part1 'W' n (x, y) (dx, dy) = ((x-n, y), (dx, dy))

part2 'N' n (x, y) (dx, dy) = ((x, y), (dx, dy+n))
part2 'S' n (x, y) (dx, dy) = ((x, y), (dx, dy-n))
part2 'E' n (x, y) (dx, dy) = ((x, y), (dx+n, dy))
part2 'W' n (x, y) (dx, dy) = ((x, y), (dx-n, dy))


parse :: String -> (Char, Int)
parse (c:cs) = (c, read cs)

example = "F10\nN3\nF7\nR90\nF11"

main :: IO ()
main = do
  instructions <- map parse . lines <$> readFile "day12.txt"
  print $ solve part1 (1,0) instructions
  print $ solve part2 (10,1) instructions
