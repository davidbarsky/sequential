module Lib.FileReader
  ( readContents
  , allFileNames
  , generateName
  ) where

import Data.Text (lines, unpack)
import Data.Text.IO (readFile)
import Protolude

generateName :: Int -> Text
generateName n
  | n < 10 = baseDir <> "etcd_00" <> show n <> ".log"
  | n >= 10 && n < 100 = baseDir <> "etcd_0" <> show n <> ".log"
  | otherwise = baseDir <> "etcd_" <> show n <> ".log"

allFileNames :: [Text]
allFileNames = map generateName [1 .. 102]

baseDir :: Text
baseDir = "/Users/David/Developer/Haskell/sequential/test-data/"

readContents :: Text -> IO [Text]
readContents a = do
  ls <- fmap lines (readFile (unpack a))
  return ls
