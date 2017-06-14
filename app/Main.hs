module Main where

import Data.Attoparsec.Text
import Lib.FileReader (readContents, generateName)
import Lib.Parser (parseLogs, Entry)
import Protolude
import Data.Text (pack)

main :: IO ()
main = do
  lines <- readContents (generateName 1)
  let entries = runParser lines
  mapM_ print entries

runParser :: [Text] -> [Entry]
runParser lines = do
  res <- map (parseOnly parseLogs) lines
  case res of
    Left err -> error ("Parse Failure at: " <> pack err)
    Right logs -> return logs
