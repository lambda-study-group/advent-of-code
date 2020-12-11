module Main where

import Data.Array
import Control.Comonad

data Seats = Floor | Empty | Occupied
           deriving (Eq, Show)

data Board a = Board { focus :: (Int, Int)          -- coordenada do foco atual
                     , board :: Array (Int, Int) a  -- dados que contém os focos
                     } 

instance Functor Board where
  fmap f (Board fc b) = Board fc (fmap f b)

instance Comonad Board where
  extract (Board ix b) = b ! ix
  extend f (Board ix b) = Board ix b'
    where
      g i = (i, f (Board i b))
      b'  = array (bounds b) $ map g (indices b)

within :: Ord a => (a,a) -> ((a, a), (a,a)) -> Bool
within (x,y) ((a,b), (c,d)) = x >= a && y >= b && x <= c && y <= d 

to :: (Int -> Int) -> (Int -> Int) -> Board a -> Maybe (Board a)
to f g (Board (x,y) b)
  | c' `within` bounds b = Just $ Board c' b
  | otherwise            = Nothing
  where
    c' = (f x, g y)

stay, right, left, up, down :: Board a -> Maybe (Board a)
stay  = to id id
right = to id (+1)
left  = to id (subtract 1)
up    = to (subtract 1) id
down  = to (+1) id

upright, upleft, downright, downleft :: Board a -> Maybe (Board a)
upright   = to (subtract 1) (+1)
upleft    = to (subtract 1) (subtract 1)
downright = to (+1) (+1)
downleft  = to (+1) (subtract 1)

neighbor :: ((Board a -> Maybe (Board a)) -> t -> b) -> t -> [[b]]
neighbor f b = map (map (`f` b)) neighs
  where
    neighs = [[upleft,   up,   upright]
             ,[left,     stay, right]
             ,[downleft, down, downright]]

immediate, first :: (Board Seats -> Maybe (Board Seats)) -> Board Seats -> Seats
immediate f b = maybe Empty extract (f b)

first f b = case f b of
                 Just b' -> if extract b' == Floor && focus b /= focus b'
                               then first f b'
                               else extract b'
                 Nothing -> Empty

changeState :: Int -> [[Seats]] -> Seats
changeState _ [row1, [s4,Empty,s6], row2]
    = if Occupied `elem` (s4:s6:row1++row2)
         then Empty
         else Occupied
changeState maxSeats [row1, [s4,Occupied,s6], row2]
    = if length (filter (==Occupied) (s4:s6:row1++row2)) >= maxSeats
         then Empty
         else Occupied
changeState _ [_, [_, s, _], _] = s

parseChar :: Char -> Seats
parseChar 'L' = Empty
parseChar '.' = Floor
parseChar '#' = Occupied
parseChar c   = error $ "no parse for " ++ [c]

parseData :: [String] -> [[Seats]]
parseData = map (map parseChar)

toBoard :: [[Seats]] -> Board Seats
toBoard xss = Board (0,0) $ listArray ((0,0), (x-1,y-1)) $ concat xss
  where
    x = length xss
    y = length $ head xss

numberOfSeats :: Board Seats -> Int
numberOfSeats = length . filter (==Occupied) . elems . board

findFirstRepeating :: [Int] -> Int
findFirstRepeating (x:xs) = go x xs
  where
    go x [] = x
    go x (y:ys)
      | x==y      = y
      | otherwise = go y ys

findSolution :: Int -> ((Board a -> Maybe (Board a)) -> Board Seats -> Seats) -> Board Seats -> Int
findSolution maxSeats sight seats = 
  let nextState = extend (changeState maxSeats . neighbor sight)
      states    = iterate nextState seats
      number    = map numberOfSeats states
  in findFirstRepeating number

main :: IO ()
main = do
  contents <- lines <$> readFile "day11.txt"
  let seats = toBoard $ parseData contents
  print $ findSolution 4 immediate seats 
  print $ findSolution 5 first seats
