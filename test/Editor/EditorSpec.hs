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
    spec_deleteLineAt
    spec_deleteLine

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

spec_deleteLineAt :: Spec
spec_deleteLineAt = describe "deleteLineAt" $ do
    it "deletes the EditorLine at the given position, and sets editor to dirty" $ do
        let e = testEditor { eLines = Seq.fromList $
                           [newELine "Hello", newELine "world"]
                           }
        deleteLineAt e 0 `shouldBe`
            testEditor { eLines = Seq.singleton $ newELine "world"
                       , eDirty = True
                       }

        deleteLineAt e 1 `shouldBe`
            testEditor { eLines = Seq.singleton $ newELine "Hello"
                       , eDirty = True
                       }

spec_deleteLine :: Spec
spec_deleteLine = describe "deleteLine" $ do
    it "deletes the EditorLine at the current y position" $ do
        let e = testEditor { eLines = Seq.fromList $
                           [newELine "Hello", newELine "world"]
                           }
        deleteLine e `shouldBe`
            e { eLines = Seq.singleton $ newELine "world"
              , eDirty = True
              }

        deleteLine (e { ecy = 1 }) `shouldBe`
            e { eLines = Seq.singleton $ newELine "Hello"
              , eDirty = True
              , ecy = 1
              }
