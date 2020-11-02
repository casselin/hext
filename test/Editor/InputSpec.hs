module Editor.InputSpec where

import Test.Hspec

import Editor.Input
import Editor.Types

testEditor :: Editor
testEditor = Editor
    { ecx = 0
    , ecy = 0
    , erx = 0
    , eColOffset = 0
    , eRowOffset = 0
    , eScreenCols = 5
    , eScreenRows = 5
    , eNumRows = 20
    , eRows =
        [EditorRow
            (length s) (length s) s s | i <- [1..20] :: [Int]
                                      , let s = "Test line" ++ show i]
    , eFileName = "test file"
    , eMessageBar = newMessageBar
    }

spec :: Spec
spec = do
    spec_unctrlkey
    spec_vertScroll

spec_unctrlkey :: Spec
spec_unctrlkey = describe "unctrlkey" $ do
    it "returns the letter of the given control character" $ do
        unctrlkey '\1' `shouldBe` 'a'
        unctrlkey '\10' `shouldBe` 'j'
        unctrlkey '\26' `shouldBe` 'z'

spec_vertScroll :: Spec
spec_vertScroll = describe "vertScroll" $ do
    it "Moves the row offset up when the row coordinate is above the window" $ do
        let e = testEditor { ecy = 2, eRowOffset = 5 }
        vertScroll e `shouldBe` e { eRowOffset = 2 }
    it "Moves the row offset down when the row coordinate is beyond the bottom of the window" $ do
        let e = testEditor { eRowOffset = 5, ecy = 12 }
        vertScroll e `shouldBe` e { eRowOffset = 12 - eScreenRows e + 1}
    it "Does nothing when the row coordinate is inside the window" $ do
        let e = testEditor { eRowOffset = 1, ecy = 4 }
        vertScroll e `shouldBe` e

spec_horiScroll :: Spec
spec_horiScroll = undefined
