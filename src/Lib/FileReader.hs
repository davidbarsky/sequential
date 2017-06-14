module Lib.FileReader
  ( logFile
  , readLogContents
  ) where

import Data.Text as Text
import Data.Text.IO as Text
import Protolude

data Place
  = Ones
  | Tens
  | Hundreds
  | OutOfScope
  deriving (Eq, Show)

logFile :: FilePath
logFile = "/Users/David/Developer/Haskell/sequential/test-data/etcd_000.log"

readLogContents :: FilePath -> IO [Text]
readLogContents a = do
  ls <- fmap Text.lines (Text.readFile a)
  return ls
