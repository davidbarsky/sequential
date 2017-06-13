{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Attoparsec.Text as AT
import Lib.FileReader as FR
import Lib.Parser as Parser
import Protolude
import Data.Text (pack)

main :: IO ()
main = do
  lines :: [Text] <- FR.readLogContents (FR.logFile)
  let entries = runParser lines
  mapM_ print entries

runParser :: [Text] -> [Entry]
runParser lines = do
  res <- map (parseOnly Parser.parseLogs) lines
  case res of
    Left err -> error ("Parse Failure at: " <> pack err)
    Right logs -> return logs
