import Protolude

import Data.Attoparsec.Text
import Lib.Parser as Parser
import Lib.FileReader as FileReader
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    let line = "INFO  jepsen.util - 0 :invoke :read nil"
    describe "Parser.logEntryParser" $ do
      it "parses a basic line correctly" $ do
        let r = parseOnly Parser.logEntryParser line
        r `shouldBe`
          Right
            (Entry
             { process = 0.0
             , requestType = Invoke
             , operation = Read
             , result = "nil"
             })
