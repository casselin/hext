module Editor.InputSpec where

import qualified Data.Sequence as Seq
import Test.Hspec

import Editor.Input
import Editor.Editor
import Editor.Line
import Editor.MessageBar


testEditor :: Editor
testEditor = Editor
    { ecx = 0
    , ecy = 0
    , erx = 0
    , eColOffset = 0
    , eRowOffset = 0
    , eScreenCols = 5
    , eScreenRows = 5
    , eLines = Seq.empty
    , eFileName = ""
    , eMessageBar = emptyMessageBar
    }

spec :: Spec
spec = do
    spec_unctrlkey
    spec_vertScroll
    spec_horiScroll
    spec_updateErx
    spec_moveCursor
    spec_snapCursor
    spec_insertChar

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

    it "Moves the row offset down when the row coordinate is below the bottom of the window" $ do
        let e = testEditor { eRowOffset = 5, ecy = 12, eScreenRows = 5 }
        vertScroll e `shouldBe` e { eRowOffset = 12 - eScreenRows e + 1}

    it "Does nothing when the row coordinate is inside the window" $ do
        let e = testEditor { eRowOffset = 1, ecy = 4, eScreenRows = 5 }
        vertScroll e `shouldBe` e

spec_horiScroll :: Spec
spec_horiScroll = describe "horiScroll" $ do
    it "Moves the column offset right when the rendered x coordinate is beyond the right of the window" $ do
        let e = testEditor { erx = 2, eColOffset = 5 }
        horiScroll e `shouldBe` e { eColOffset = 2 }

    it "Moves the column offset left when the rendered x coordinate is beyond the left of the window" $ do
        let e = testEditor { eColOffset = 5, erx = 12, eScreenCols = 5 }
        horiScroll e `shouldBe` e { eColOffset = 12 - eScreenCols e + 1}

    it "Does nothing when the column coordinate is inside the window" $ do
        let e = testEditor { eColOffset = 1, erx = 4, eScreenCols = 5 }
        horiScroll e `shouldBe` e

spec_updateErx :: Spec
spec_updateErx = describe "updateErx" $ do
    it "Updates the editor's render x coordinate based on the editor's file x coordinate (treating tabs as spaces)" $ do
        let e = testEditor { ecx = 0
                           , eLines = Seq.singleton $ newELine "\t\t"
                           }
        updateErx e `shouldBe` e { erx = 0 }
        let e1 = testEditor { ecx = 1
                            , eLines = Seq.singleton $ newELine "\t\t"
                            }
        updateErx e1 `shouldBe` e1 { erx = 8 }
        let e2 = testEditor { ecx = 2
                            , eLines = Seq.singleton $ newELine "\t\t"
                            }
        updateErx e2 `shouldBe` e2 { erx = 16 }

spec_moveCursor :: Spec
spec_moveCursor = describe "moveCursor" $ do
    it "Moves the cursor up" $ do
        let e = testEditor { ecy = 1 }
        moveCursor e ArrowUp `shouldBe` e { ecy = 0 }

    it "Prevents moving the cursor up when at the top line" $ do
        let e = testEditor { ecy = 0 }
        moveCursor e ArrowUp `shouldBe` e

    it "Moves the cursor down" $ do
        let e = testEditor { eLines = Seq.singleton $ newELine "" }
        moveCursor e ArrowDown `shouldBe` e { ecy = 1 }

    it "Prevents moving the cursor down when at the bottom line" $ do
        let e = testEditor { ecy = 0 }
        moveCursor e ArrowDown `shouldBe` e

    it "Moves the cursor left" $ do
        let e = testEditor { ecx = 1
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e ArrowLeft `shouldBe` e { ecx = 0 }

    it "Moves the cursor to the end of the previous line if moving left at the beginning of a line" $ do
        let e = testEditor { ecx = 0
                           , ecy = 1
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e ArrowLeft `shouldBe` e { ecy = 0, ecx = 11 }

    it "Prevents moving the cursor if at the beginning of the first line" $ do
        let e = testEditor { ecx = 0
                           , ecy = 0
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e ArrowLeft `shouldBe` e

    it "Moves the cursor right" $ do
        let e = testEditor { ecx = 0
                           , ecy = 0
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e ArrowRight `shouldBe` e { ecx = 1 }

    it "Moves the cursor to the beginning of the next line if moving right at the end of a line" $ do
        let e = testEditor { ecx = 11
                           , ecy = 0
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e ArrowRight `shouldBe` e { ecy = 1, ecx = 0 }

    it "Does nothing if moving right when one line below the end of the file" $ do
        let e = testEditor { ecx = 0
                           , ecy = 2
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e ArrowRight `shouldBe` e

    it "Moves cursor to beginning of the current line" $ do
        let e = testEditor { ecx = 5
                           , ecy = 0
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e HomeKey `shouldBe` e { ecx = 0 }

    it "moves cursor to the end of the current line" $ do
        let e = testEditor { ecx = 0
                           , ecy = 0
                           , eLines = Seq.fromList [newELine "Test line 1",
                                                    newELine "Test line 2"]
                           }
        moveCursor e EndKey `shouldBe` e { ecx = 11 }

spec_snapCursor :: Spec
spec_snapCursor = describe "snapCursor" $ do
    it "Moves the x coordinate to the end of the line if the x coordinate is too far to the right" $ do
        let e = testEditor { eLines = Seq.singleton $ newELine "Test line 1"
                           , ecx = 15
                           }
        snapCursor e `shouldBe` e { ecx = 11 }

    it "Does nothing if the x coordinate is within the text of the line" $ do
        let e = testEditor { eLines = Seq.singleton $ newELine "Test line 1"
                           , ecx = 5
                           }
        snapCursor e `shouldBe` e

spec_insertChar :: Spec
spec_insertChar = describe "insertChar" $ do
    it "inserts a character into the EditorLine at the current x and y position and moves x to the right" $ do
        let e = testEditor { eLines = Seq.singleton $ newELine "Hello orld"
                           , ecx = 6
                           }
        insertChar e 'w' `shouldBe`
            updateCursor ArrowRight
                e { eLines = Seq.singleton $ newELine "Hello world" }

    it "appends a new EditorLine and inserts the character if at the end of the file and moves x to the right" $ do
        let e = testEditor
        insertChar e 'a' `shouldBe`
            updateCursor ArrowRight
                e { eLines = (eLines e) Seq.|> (newELine "a") }
