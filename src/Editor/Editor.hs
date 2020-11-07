module Editor.Editor where

import Data.Time.Clock.System (SystemTime)

import Editor.Line
import Editor.MessageBar


type Rows = Int
type Cols = Int

data Editor = Editor
    { ecx            :: Cols
    , ecy            :: Rows
    , erx            :: Cols
    , eColOffset     :: Cols
    , eRowOffset     :: Rows
    , eScreenCols    :: Cols
    , eScreenRows    :: Rows
    , eNumLines      :: Rows
    , eLines         :: [EditorLine]
    , eFileName      :: String
    , eMessageBar    :: MessageBar
    }
    deriving (Show, Eq)

newEditor :: Editor
newEditor = Editor
    { ecx            = 0
    , ecy            = 0
    , erx            = 0
    , eColOffset     = 0
    , eRowOffset     = 0
    , eScreenCols    = 0
    , eScreenRows    = 0
    , eNumLines      = 0
    , eLines         = []
    , eFileName      = ""
    , eMessageBar    = emptyMessageBar
    }

appendLine :: Editor -> String -> Editor
appendLine e s = e { eNumLines = succ $ eNumLines e
                   , eLines = eLines e ++ [updateRender $ newELine s]
                   }

setMessageBar :: Editor -> String -> SystemTime -> Editor
setMessageBar e s t = e { eMessageBar = newMessageBar s t }

newtype EscSeq = EscSeq { unEscSeq :: String }
    deriving (Show, Eq)

data KeyPress
    = Letter Char
    | Ctrl Char
    | Escape Direction
    deriving (Show, Eq)

data Direction
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | DelKey
    | PageUp
    | PageDown
    | HomeKey
    | EndKey
    deriving (Show, Eq)
