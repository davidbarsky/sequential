module Main where

import Data.Attoparsec.Text
import Data.Text (pack)
import Lib.FileReader (generateName, readContents)
import Lib.Parser (Entry, parseLogs)
import Protolude

main :: IO ()
main = do
  lines <- mapM readContents generateFileNames
  let entries = map runParser lines
  mapM_ printLog entries

generateFileNames :: [Text]
generateFileNames = map generateName [1 .. 102]

printLog :: [Entry] -> IO ()
printLog entries = mapM_ print entries

runParser :: [Text] -> [Entry]
runParser lines = do
  res <- map (parseOnly parseLogs) lines
  case res of
    Left err -> error ("Parse Failure at: " <> pack err)
    Right logs -> return logs
