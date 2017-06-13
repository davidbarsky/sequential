module Lib.Parser (
    parseLogs
  , Entry
) where

import Data.Attoparsec.Text as AT
import Control.Applicative
import Lib.Prelude

data RequestType
  = Ok
  | Invoke
  | Failure
  | Info
  deriving (Eq, Show)

-- either read, write, compareAndSwap
data Operation
  = Read
  | Write
  | CompareAndSwap
  deriving (Eq, Show)

-- a jepsen log entry
data Entry = Entry
  { process :: Double
  , requestType :: RequestType
  , operation :: Operation
  , response :: Text
  } deriving (Eq, Show)

parseLogs :: Parser Entry
parseLogs = do
  _ <- string "INFO  jepsen.util - "
  id <- processIDParser
  _ <- space
  rt <- requestTypeParser
  _ <- space
  op <- operationParser
  r <- responseParser
  return $ Entry {process = id, requestType = rt, operation = op, response = r}

processIDParser :: Parser Double
processIDParser = do
  processID <- double
  return processID

requestTypeParser :: Parser RequestType
requestTypeParser =
      (string ":ok" >> return Ok)
  <|> (string ":invoke" >> return Invoke)
  <|> (string ":fail" >> return Failure)
  <|> (string ":info" >> return Info)

operationParser :: Parser Operation
operationParser =
      (string ":read" >> return Read)
  <|> (string ":write" >> return Write)
  <|> (string ":cas" >> return CompareAndSwap)

responseParser :: Parser Text
responseParser = do
  _ <- space
  r <- AT.takeTill (\c -> c == '}')
  return r