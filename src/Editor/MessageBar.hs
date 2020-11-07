module Editor.MessageBar where

import GHC.Int (Int64)
import Data.Time.Clock.System
    (SystemTime( MkSystemTime
               , systemSeconds
               )
    )


data MessageBar = MessageBar
    { mbContents :: String
    , mbTime     :: SystemTime
    }
    deriving (Show, Eq)

timeLimit :: Int64
timeLimit = 5

newMessageBar :: String -> SystemTime -> MessageBar
newMessageBar s t = MessageBar
    { mbContents = s
    , mbTime     = t
    }

emptyMessageBar :: MessageBar
emptyMessageBar = MessageBar "" (MkSystemTime 0 0)

inTime :: MessageBar -> SystemTime -> Bool
inTime mb t = systemSeconds t - systemSeconds start < timeLimit
    where start = mbTime mb
