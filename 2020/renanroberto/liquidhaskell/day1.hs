module Day1 where


first :: [a] -> Maybe a
first []    = Nothing
first (x:_) = Just x

{-@ measure sum' @-}
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs


{-@
solve1 :: t:Int -> [Int] -> Maybe {v:[Int] | len v == 2 && sum' v == t}
@-}
solve1 :: Int -> [Int] -> Maybe [Int]
solve1 target xs = first [[x, y] | x <- xs, y <- xs, x /= y, x + y == target]

{-@
solve2 :: t:Int -> [Int] -> Maybe {v:[Int] | len v == 3 && sum' v == t}
@-}
solve2 :: Int -> [Int] -> Maybe [Int]
solve2 target xs = first
  [ [x, y, z]
  | x <- xs
  , y <- xs
  , z <- xs
  , x /= y
  , y /= z
  , z /= x
  , x + y + z == target
  ]


main :: IO ()
main = do
  putStrLn "Input file: "
  filepath <- getLine
  input <- readFile filepath
  let nums = (fmap read . lines) input
  (print . fmap product . solve1 2020) nums
  (print . fmap product . solve2 2020) nums
