module Common
  ( readFileUtf8
  ) where

import System.IO (withFile, hSetEncoding, utf8, IOMode(ReadMode))
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  TextIO.hGetContents h
