module Editor.RowSpec where

import Test.Hspec

import Editor.Row
import Editor.Editor


testEditor :: Editor
testEditor = newEditor

testRow :: EditorRow
testRow = EditorRow
    { rowSize = 11
    , rowContents = "Hello world"
    , rowRenderSize = 11
    , rowRender = "Hello world"
    }

spec :: Spec
spec = do
    spec_rowInsertChar
    spec_nextTab
    spec_ecxToErx
    spec_removeTabs
    spec_updateRender

-- TO BE MOVED TO EDITOR.HS
-- spec_appendRow :: Spec
-- spec_appendRow = describe "appendRow" $ do
--     it "appends the given string to the given editor's EditorRows" $ do
--         appendRow testEditor "Hello world" `shouldBe`
--             (newEditor { eNumRows = 1
--                        , eRows =
--                            [EditorRow 11 11 "Hello world" "Hello world"]
--                        })

spec_rowInsertChar :: Spec
spec_rowInsertChar = describe "rowInsertChar" $ do
    it "inserts character at given position of given EditorRow" $ do
        rowInsertChar (newERow "hllo") 1 'e'
            `shouldBe` EditorRow 5 5 "hello" "hello"
        rowInsertChar (newERow "ello") 0 'h'
            `shouldBe` EditorRow 5 5 "hello" "hello"
        rowInsertChar (newERow "hell") 4 'o'
            `shouldBe` EditorRow 5 5 "hello" "hello"
        rowInsertChar (newERow "hell") 3 'o'
            `shouldBe` EditorRow 5 5 "helol" "helol"

    it "prepends if index is too small" $ do
        rowInsertChar (newERow "ello") (-1) 'h'
            `shouldBe` EditorRow 5 5 "hello" "hello"

    it "appends if index is too large" $ do
        rowInsertChar (newERow "hell") 10 'o'
            `shouldBe` EditorRow 5 5 "hello" "hello"

spec_nextTab :: Spec
spec_nextTab = describe "nextTab" $ do
    it "returns the number of columns to advance to next tab stop" $ do
        nextTab 0 `shouldBe` 8
        nextTab 1 `shouldBe` 7
        nextTab 2 `shouldBe` 6
        nextTab 13 `shouldBe` 3
        nextTab 23 `shouldBe` 1

spec_ecxToErx :: Spec
spec_ecxToErx = describe "ecxToErx" $ do
    it "converts column position in text to column position on screen (so that tabs are rendered as spaces)" $ do
        ecxToErx (newERow "\t") 0 `shouldBe` 0
        ecxToErx (newERow "\t") 1 `shouldBe` 8
        ecxToErx (newERow "01\t") 3 `shouldBe` 8
        ecxToErx (newERow "\t\t") 1 `shouldBe` 8
        ecxToErx (newERow "\t\t") 2 `shouldBe` 16

spec_removeTabs :: Spec
spec_removeTabs = describe "removeTabs" $ do
    it "converts tabs to the appropriate number of spaces" $ do
        removeTabs "\t" `shouldBe` "        "
        removeTabs "1\t2" `shouldBe` "1       2"
        removeTabs "1234567\t" `shouldBe` "1234567 "
        removeTabs "1\t2\t" `shouldBe` "1       2       "

    it "does nothing if the string has no tab characters" $ do
        removeTabs "1234" `shouldBe` "1234"

spec_updateRender :: Spec
spec_updateRender = describe "updateRender" $ do
    it "updates the renderSize and rowRender fields of an EditorRow" $ do
        updateRender (EditorRow 11 0 "Hello world" "") `shouldBe`
            (EditorRow 11 11 "Hello world" "Hello world")
