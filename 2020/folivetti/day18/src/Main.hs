module Main where

import Data.Char

data Op = Nop | Rgt | Mul | Add | Lft deriving (Show, Eq, Ord)

evalExpr :: Bool -> String -> Integer
evalExpr p expr = go expr [] []
  where
    go []     vals []  = head vals
    go []     vals ops = head $ fst $ update p Nop vals ops
    go (c:cs) vals ops
      | isDigit c = go cs (read [c]:vals) ops
      | c == '('  = go cs vals (Lft:ops)
      | c == ')'  = uncurry (go cs) $ update p Rgt vals ops
      | c == '+'  = uncurry (go cs) $ update p Add vals ops
      | c == '*'  = uncurry (go cs) $ update p Mul vals ops
      | otherwise = go cs vals ops

update :: Bool -> Op -> [Integer] -> [Op] -> ([Integer],[Op])
-- make Add and Mul have the same precedence
update False Add vals (Mul:ops) = update False Add (calc Mul vals) ops

update _ cur vals []        = (vals, [cur])
update _ Rgt vals (Lft:ops) = (vals, ops)
update _ cur vals (Lft:ops) = (vals, cur:Lft:ops)
update p cur vals (op:ops)
  | op >= cur = update p cur (calc op vals) ops
  | otherwise = (vals, cur:op:ops)

calc Add (v1:v2:vs) = v1+v2:vs
calc Mul (v1:v2:vs) = v1*v2:vs

main :: IO ()
main = do
  exprs <- lines <$> readFile "day18.txt"
  print $ foldr (\e acc -> evalExpr False e + acc) 0 exprs
  print $ foldr (\e acc -> evalExpr True e + acc) 0 exprs
