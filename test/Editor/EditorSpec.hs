module Editor.EditorSpec where

import Test.Hspec

import qualified Data.Sequence as Seq
import Data.Time.Clock.System (SystemTime(MkSystemTime))

import Editor.Editor
import Editor.Line
import Editor.MessageBar


testEditor :: Editor
testEditor = newEditor

spec :: Spec
spec = do
    spec_appendLine
    spec_eNumLines
    spec_setMessageBar

spec_eNumLines :: Spec
spec_eNumLines = describe "eNumLines" $ do
    it "returns the length of the eLines field" $ do
        eNumLines testEditor `shouldBe` 0
        let e = testEditor { eLines = Seq.fromList $
                                replicate 4 (newELine "") }
        eNumLines e `shouldBe` 4

spec_appendLine :: Spec
spec_appendLine = describe "appendLine" $ do
    it "appends the given string to the given editor's EditorLines and sets the editor's status to dirty" $ do
        appendLine testEditor "Hello world" `shouldBe`
            testEditor { eLines = Seq.fromList $
                [EditorLine 11 11 "Hello world" "Hello world"]
                       , eDirty = True
                       }

    it "increases the length of the eLines field by one" $ do
        (length . eLines . appendLine testEditor) "Hello world" `shouldBe`
            ((+ 1) . length . eLines) testEditor

spec_setMessageBar :: Spec
spec_setMessageBar = describe "setMessageBar" $ do
    it "returns an editor with a MessageBar with the given string and time" $ do
        let t = MkSystemTime 10 10
            e = testEditor { eTime = t }
        setMessageBar e "Hello" `shouldBe`
            e { eMessageBar = newMessageBar "Hello" t }
