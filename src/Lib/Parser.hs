module Lib.Parser where

import Data.Attoparsec.Text (Parser, string, space, decimal)
import Control.Applicative
import Protolude

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


data Response
  = TimedOut
  | Pair Integer Integer
  | Nil
  | SingleDigit Integer
  deriving (Eq, Show)


data Entry = Entry
  { process :: Integer
  , requestType :: RequestType
  , operation :: Operation
  , response :: Response
  } deriving (Eq, Show)


parseLogs :: Parser Entry
parseLogs = do
  _ <- string "INFO  jepsen.util - "
  id <- processIDParser
  _ <- space
  rt <- requestTypeParser
  _ <- space
  op <- operationParser
  _ <- space
  r <- responseParser
  return $ Entry {process = id, requestType = rt, operation = op, response = r}


processIDParser :: Parser Integer
processIDParser = do
  processID <- decimal
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


responseParser :: Parser Response
responseParser = 
      singleDigitParser
  <|> tupleParser
  <|> timedOutParser
  <|> nilParser

nilParser :: Parser Response 
nilParser = do
  _ <- string "nil"
  return Nil


timedOutParser :: Parser Response
timedOutParser = do
  _ <- string ":timed-out"
  return TimedOut


singleDigitParser :: Parser Response
singleDigitParser = do
  d :: Integer <- decimal
  return (SingleDigit d)


tupleParser :: Parser Response
tupleParser = do
  _ <- string "["
  firstInTuple :: Integer <- decimal
  _ <- space
  secondInTuple :: Integer <- decimal
  _ <- string "]"
  return (Pair firstInTuple secondInTuple)