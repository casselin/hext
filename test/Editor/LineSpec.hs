module Editor.LineSpec where

import Test.Hspec

import Editor.Line


testLine :: EditorLine
testLine = EditorLine
    { lineSize = 11
    , lineContents = "Hello world"
    , lineRenderSize = 11
    , lineRender = "Hello world"
    }

spec :: Spec
spec = do
    spec_insertCharAt
    spec_deleteCharAt
    spec_appendString
    spec_lineAppend
    spec_nextTab
    spec_ecxToErx
    spec_removeTabs
    spec_updateRender

spec_insertCharAt :: Spec
spec_insertCharAt = describe "insertCharAt" $ do
    it "inserts character at given position of given EditorLine" $ do
        insertCharAt (newELine "hllo") 1 'e'
            `shouldBe` EditorLine 5 5 "hello" "hello"
        insertCharAt (newELine "ello") 0 'h'
            `shouldBe` EditorLine 5 5 "hello" "hello"
        insertCharAt (newELine "hell") 4 'o'
            `shouldBe` EditorLine 5 5 "hello" "hello"
        insertCharAt (newELine "hell") 3 'o'
            `shouldBe` EditorLine 5 5 "helol" "helol"

    it "prepends if index is too small" $ do
        insertCharAt (newELine "ello") (-1) 'h'
            `shouldBe` EditorLine 5 5 "hello" "hello"

    it "appends if index is too large" $ do
        insertCharAt (newELine "hell") 10 'o'
            `shouldBe` EditorLine 5 5 "hello" "hello"

    it "increases the length of the EditorLine" $ do
        (lineSize . insertCharAt (newELine "124") 2) '3'
            `shouldBe` 4

spec_deleteCharAt :: Spec
spec_deleteCharAt = describe "deleteCharAt" $ do
    it "deletes the character at the given index" $ do
        deleteCharAt (newELine "hello") 1 `shouldBe`
            newELine "hllo"

    it "deletes the first element if the index is negative" $ do
        deleteCharAt (newELine "hello") (-1) `shouldBe`
            newELine "ello"

    it "returns the same EditorLine if the index is too large" $ do
        deleteCharAt (newELine "hello") 5 `shouldBe`
            newELine "hello"

    it "reduces the length of the EditorLine by 1" $ do
        (lineSize . deleteCharAt (newELine "1243")) 2 `shouldBe` 3

spec_appendString :: Spec
spec_appendString = describe "appendString" $ do
    it "appends the given string to the given EditorLine" $ do
        appendString (newELine "Hello ") "world" `shouldBe`
            newELine "Hello world"

spec_lineAppend :: Spec
spec_lineAppend = describe "lineAppend" $ do
    it "appends the second EditorLine's contents to the contents of the first" $ do
        let l1 = newELine "Hello "
            l2 = newELine "world"
        lineAppend l1 l2 `shouldBe` newELine "Hello world"

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
        ecxToErx (newELine "\t") 0 `shouldBe` 0
        ecxToErx (newELine "\t") 1 `shouldBe` 8
        ecxToErx (newELine "01\t") 3 `shouldBe` 8
        ecxToErx (newELine "\t\t") 1 `shouldBe` 8
        ecxToErx (newELine "\t\t") 2 `shouldBe` 16

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
    it "updates the renderSize and lineRender fields of an EditorLine" $ do
        updateRender (EditorLine 11 0 "Hello world" "") `shouldBe`
            (EditorLine 11 11 "Hello world" "Hello world")
