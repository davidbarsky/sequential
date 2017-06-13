{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude
import Lib.FileReader as FR
import Lib.Parser as P
import Data.Attoparsec.Text as AT
import GHC.Base hiding (map)

main :: IO ()
main = do
  lines :: [Text] <- FR.readLogContents (FR.logFile)
  let entries = runParser lines
  mapM_ print entries

runParser :: [Text] -> [Entry]
runParser lines = do
  res <- map (parseOnly P.parseLogs) lines
  case res of 
    Left err -> fail ("Log parser failure at: " <> err)
    Right logs -> return logs