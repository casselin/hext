module Editor.FileIOSpec where

import qualified Data.Sequence as Seq (empty)
import Test.Hspec

import Editor.FileIO
import Editor.Editor


spec :: Spec
spec = do
    spec_clearFile
    spec_setFileName
    spec_loadContents
    spec_loadFile
    spec_linesToString


spec_clearFile :: Spec
spec_clearFile = describe "clearFile" $ do
    it "clears the eLines and eFileName fields of an editor" $ do
        let e = appendLine (newEditor { eFileName = "test" }) "hello world"
        clearFile e `shouldBe` e { eFileName = "", eLines = Seq.empty }

spec_setFileName :: Spec
spec_setFileName = describe "setFileName" $ do
    it "sets the eFileName field to the given string for the given editor" $ do
        setFileName newEditor "test" `shouldBe`
            newEditor { eFileName = "test" }

spec_loadContents :: Spec
spec_loadContents = describe "loadContents" $ do
    it "splits the given string into lines and appends it to the eLines field of the given editor" $ do
        let e = appendLine newEditor "Line 1"
        loadContents e "Line 2\nLine 3" `shouldBe`
            appendLine (appendLine e "Line 2") "Line 3"

spec_loadFile :: Spec
spec_loadFile = describe "loadFile" $ do
    it "updates an editor's eLines field to contain the lines of the given string" $ do
        loadFile newEditor "" "Hello world" `shouldBe`
            appendLine newEditor "Hello world"

    it "updates the eFileName field of the editor to the given FilePath" $ do
        loadFile newEditor "test" "Hello world" `shouldBe`
            (appendLine newEditor "Hello world") { eFileName = "test" }

    it "replaces the previous contents of the editor's eLines field" $ do
        let e = appendLine newEditor "Hello world"
        loadFile e "test" "hello world!" `shouldBe`
            (appendLine newEditor "hello world!") { eFileName = "test" }

spec_linesToString :: Spec
spec_linesToString = describe "linesToString" $ do
    it "appends a newline character on each eLine then concatenates each eLine" $ do
        let e = appendLine (appendLine newEditor "Line 1") "Line 2"
        linesToString e `shouldBe` "Line 1\nLine 2\n"
