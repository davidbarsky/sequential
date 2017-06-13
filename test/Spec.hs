import Protolude

import Data.Attoparsec.Text
import Lib.Parser as Parser
import Test.Hspec

headEntry :: Entry
headEntry =
  Entry {process = 0, requestType = Invoke, operation = Read, response = "nil"}

main :: IO ()
main =
  hspec $ do
    let line = "INFO  jepsen.util - 0 :invoke :read nil"
    describe "Parser.logEntryParser" $ do
      it "parses a sample line correctly" $ do
        let r = parseOnly Parser.parseLogs line
        r `shouldBe` Right headEntry
