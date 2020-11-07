module Editor.MessageBarSpec where

import Data.Time.Clock.System
import Test.Hspec

import Editor.MessageBar


spec :: Spec
spec = do
    spec_newMessageBar
    spec_inTime

spec_newMessageBar :: Spec
spec_newMessageBar = describe "newMessageBar" $ do
    it "returns a new message bar containing the given string and time" $ do
        t <- getSystemTime
        newMessageBar "Hello world!" t `shouldBe`
            MessageBar "Hello world!" t

spec_inTime :: Spec
spec_inTime = describe "inTime" $ do
    it "returns true if given SystemTime is within 5 seconds of message bar's SystemTime" $ do
        let t = MkSystemTime 4 0
            mb = MessageBar "" (MkSystemTime 0 0)
        inTime mb t `shouldBe` True

    it "returns false if the given SystemTime is 5 or more seconds later than the message bar's SystemTime" $ do
        let t = MkSystemTime 5 0
            mb = MessageBar "" (MkSystemTime 0 0)
        inTime mb t `shouldBe` False
