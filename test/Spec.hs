import Protolude

import Data.Attoparsec.Text
import Lib.FileReader (generateName, allFileNames)
import Lib.Parser
import Test.Hspec

headEntry :: Entry
headEntry =
  Entry {process = 0, requestType = Invoke, operation = Read, response = Nil}

main :: IO ()
main = do
  parserSpec
  fileReaderSpec

parserSpec :: IO ()
parserSpec =
  hspec $ do
    let line = "INFO  jepsen.util - 0 :invoke :read nil"
    describe "Parser.logEntryParser" $ do
      it "parses a sample line correctly" $ do
        let r = parseOnly parseLogs line
        r `shouldBe` Right headEntry

fileReaderSpec :: IO ()
fileReaderSpec =
  hspec $ do
    let baseDir = "/Users/David/Developer/Haskell/sequential/test-data/"
    describe "FileReader.generateLogName" $ do
      it "generates a correct filename for n < 10" $ do
        let f = generateName 1
        f `shouldBe` baseDir <> "etcd_001.log"
      it "generates a correct filename for n >= 10 && n < 100" $ do
        let f = generateName 20
        f `shouldBe` baseDir <> "etcd_020.log"
      it "generates a correct filename for n > 100" $ do
        let f = generateName 101
        f `shouldBe` baseDir <> "etcd_101.log"

    describe "FileReader.allFileNames" $ do
      it "has a length of 102" $ do
        length allFileNames `shouldBe` 102
    