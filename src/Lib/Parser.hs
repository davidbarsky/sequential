module Lib.Parser where

import Data.Attoparsec.Text
import Lib.Prelude

data IP =
  IP Word8
     Word8
     Word8
     Word8
  deriving (Show)

-- either :ok or :invoke
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
data LogEntry = LogEntry
  { process :: Double
  , requestType :: RequestType
  , operation :: Operation
  , result :: Text
  } deriving (Eq, Show)

-- type synonym of a list of log entries
type Log = [LogEntry]

logEntryParser :: Parser LogEntry
logEntryParser = do
  _ <- string "INFO  jepsen.util - "
  id <- processIDParser
  _ <- space
  rt <- requestTypeParser
  _ <- space
  op <- operationParser
  r <- resultParser
  return $ LogEntry {process = id, requestType = rt, operation = op, result = r}

printLogs :: IO ()
printLogs = do
  print $ parseOnly logEntryParser "INFO  jepsen.util - 0 :invoke :read nil"
  print $ parseOnly logEntryParser "INFO  jepsen.util - 2 :invoke :cas [3 0]"

processIDParser :: Parser Double
processIDParser = do
  processID <- double
  return processID

requestTypeParser :: Parser RequestType
requestTypeParser =
  (string ":ok" >> return Ok) <|> (string ":invoke" >> return Invoke) <|>
  (string ":fail" >> return Failure) <|>
  (string ":info" >> return Info)

operationParser :: Parser Operation
operationParser =
  (string ":read" >> return Read) <|> (string ":write" >> return Write) <|>
  (string ":cas" >> return CompareAndSwap)

resultParser :: Parser Text
resultParser = do
  _ <- space
  r <- Data.Attoparsec.Text.takeTill (\c -> c == '}')
  return r
