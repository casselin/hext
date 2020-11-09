module Editor.EditorSpec where

import qualified Data.Sequence as Seq
import Test.Hspec

import Editor.Editor
import Editor.Line


testEditor :: Editor
testEditor = newEditor

spec :: Spec
spec = do
    spec_appendLine

spec_appendLine :: Spec
spec_appendLine = describe "appendLine" $ do
    it "appends the given string to the given editor's EditorLines" $ do
        appendLine testEditor "Hello world" `shouldBe`
            testEditor { eLines = Seq.fromList $
                [EditorLine 11 11 "Hello world" "Hello world"]
                       }

    it "increases the length of the eLines field by one" $ do
        (length . eLines . appendLine testEditor) "Hello world" `shouldBe`
            ((+ 1) . length . eLines) testEditor
