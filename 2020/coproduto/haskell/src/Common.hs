module Common
  ( readFileUtf8
  , textToInt
  , textToInteger
  , sum'
  , product'
  ) where

import Data.List (foldl')
import System.IO (withFile, hSetEncoding, utf8, IOMode(ReadMode))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as Read

sum' :: (Num a, Foldable f) => f a -> a
sum' = foldl' (+) 0

product' :: (Num a, Foldable f) => f a -> a
product' = foldl' (*) 1

textToInt :: Text -> Int
textToInt txt = let Right (x, _) = Read.decimal txt in x

textToInteger :: Text -> Integer
textToInteger txt = let Right (x, _) = Read.decimal txt in x

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  TextIO.hGetContents h
