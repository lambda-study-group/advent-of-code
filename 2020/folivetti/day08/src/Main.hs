module Main where

import Data.Char (toUpper)
import Data.Array
import Data.IntSet (member, IntSet(), insert, empty)

import Algebra

type Code = Array Int OP

data OP = NOP Int | ACC Int | JMP Int
        deriving (Show, Read)

codeSequence ::  Code -> CoAlgebra (ListF (Int, Int)) (Int, Int)
codeSequence code (loc, acc) 
  | loc > snd (bounds code) = NilF
  | otherwise = ConsF (loc, acc) next 
  where
    next = case code ! loc of
              NOP _ -> (loc+1, 0)
              ACC x -> (loc+1, x)
              JMP x -> (loc+x, 0)

runCode :: Algebra (ListF (Int, Int)) Int
runCode NilF        = 0
runCode (ConsF e n) = snd e + n

getTrace :: Algebra (ListF (Int, Int)) [(Int, Int)]
getTrace NilF        = []
getTrace (ConsF e n) = e : n

takeBeforeDup :: Algebra (ListF (Int, Int)) (IntSet -> Int)
takeBeforeDup NilF                 = const 0
takeBeforeDup (ConsF (loc, acc) n) = fs
  where
    fs set
      | loc `member` set = 0
      | otherwise        = acc + n (insert loc set)

hyloFindDup :: Code -> Int
hyloFindDup code = hylo takeBeforeDup (codeSequence code) (0,0) empty

doesItTerminate :: Algebra (ListF (Int, Int)) (IntSet -> (Bool, Int))
doesItTerminate NilF                 = const (True, 0)
doesItTerminate (ConsF (loc, acc) n) = fs
  where
    fs set
      | loc `member` set = (False, 0)
      | otherwise        = (+acc) <$> n (insert loc set)

codeBranch :: Code -> CoAlgebra (TreeF (Int, Int)) (Int, Int, Bool)
codeBranch code (loc, acc, b)
  | loc > snd (bounds code) = LeafF
  | otherwise = case code ! loc of
                     ACC x -> SingleF (loc, acc) (loc+1, x, b)
                     NOP x -> nextNode b (NOP x)
                     JMP x -> nextNode b (JMP x)
  where
    nextNode True (NOP x)  = SingleF (loc, acc) (loc+1, 0, True)
    nextNode True (JMP x)  = SingleF (loc, acc) (loc+x, 0, True)
    nextNode False (NOP x) = DoubleF (loc, acc) (loc+1, 0, False) (loc+x, 0, True)
    nextNode False (JMP x) = DoubleF (loc, acc) (loc+x, 0, False) (loc+1, 0, True)


firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust _ y        = y

codeFix :: Algebra (TreeF (Int, Int)) (IntSet -> Maybe Int)
codeFix LeafF                  = const (Just 0)
codeFix (SingleF (loc, acc) n) = fs
  where
    fs set
      | loc `member` set = Nothing 
      | otherwise        = (+acc) <$> n (insert loc set)
codeFix (DoubleF (loc, acc) l r) = fs
  where
    fs set
      | loc `member` set = Nothing
      | otherwise        = let set' = insert loc set
                               l'   = (+acc) <$> l set'
                               r'   = (+acc) <$> r set'
                           in  firstJust l' r' 


main :: IO ()
main = do
  contents <- lines . filter (/='+') . map toUpper <$> readFile "day08.txt"
  let 
    ops     = map read contents :: [OP]
    n       = length ops
    code    = array (0,n-1) $ zip [0..] ops
  print $ hyloFindDup code
  print $ hylo codeFix (codeBranch code) (0,0, False) empty
