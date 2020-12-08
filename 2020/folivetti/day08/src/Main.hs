module Main where

import Data.Char (toUpper)
import Data.Array
import qualified Data.IntSet as S

import Algebra

type Code = Array Int OP

data OP = NOP Int | ACC Int | JMP Int
        deriving (Show, Read)

runCode ::  Code -> CoAlgebra (ListF (Int, Int)) (Int, Int)
runCode code (loc, acc) 
  | loc > snd (bounds code) = NilF
  | otherwise = ConsF (loc, acc) next 
  where
    next = case code ! loc of
              NOP _ -> (loc+1, acc)
              ACC x -> (loc+1, acc+x)
              JMP x -> (loc+x, acc)

getTrace :: Algebra (ListF (Int, Int)) [(Int, Int)]
getTrace NilF        = []
getTrace (ConsF e n) = e : n

takeUntilDup :: [(Int, Int)] -> [(Int, Int)]
takeUntilDup xs = foldr go (const []) xs S.empty
  where
    -- continuation passing style, we incrementally create a function
    -- that returns the end of the list if the element is found
    -- or insert the elements and calls the continuation.
    go x cont set
      | S.member (fst x) set = []
      | otherwise            = x : cont (S.insert (fst x) set)

doesItTerminate :: [(Int, Int)] -> Bool
doesItTerminate xs = foldr go (const True) xs S.empty
  where
    go x cont set
      | S.member (fst x) set = False 
      | otherwise            = cont (S.insert (fst x) set)

isNotAcc :: (Int, OP) -> (Bool, (Int, OP))
isNotAcc (loc, op) =
  case op of
       ACC _ -> (False, (loc, op))
       NOP x -> (True, (loc, JMP x))
       JMP x -> (True, (loc, NOP x))

findPossibleChanges :: [(Int, OP)] -> [(Int, OP)]
findPossibleChanges = map snd . filter fst . map isNotAcc

createFixes :: Array Int OP -> [(Int, OP)] -> [Array Int OP]
createFixes code = map ((code//).return) . findPossibleChanges 

applyHylo :: Code -> [(Int, Int)]
applyHylo code = hylo getTrace (runCode code) (0,0)

findFix :: [Code] -> [(Int, Int)]
findFix = head . filter doesItTerminate . map applyHylo

main :: IO ()
main = do
  contents <- lines . filter (/='+') . map toUpper <$> readFile "day08.txt"
  let 
    ops     = map read contents :: [OP]
    n       = length ops
    zipped  = zip [0..] ops
    code    = array (0,n-1) zipped
    fixes   = createFixes code zipped
    trace   = applyHylo code
    term    = findFix fixes
  print $ last $ takeUntilDup trace
  print $ last term
