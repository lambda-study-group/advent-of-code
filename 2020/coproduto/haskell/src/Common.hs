module Common
  ( readFileUtf8
  , textToInt
  ) where

import System.IO (withFile, hSetEncoding, utf8, IOMode(ReadMode))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as Read

textToInt :: Text -> Int
textToInt txt = let Right (x, _) = Read.decimal txt in x

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  TextIO.hGetContents h
