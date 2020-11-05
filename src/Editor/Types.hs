module Editor.Types where

import Editor.Row
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
    , eNumRows       :: Rows
    , eRows          :: [EditorRow]
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
    , eNumRows       = 0
    , eRows          = []
    , eFileName      = ""
    , eMessageBar    = emptyMessageBar
    }

appendRow :: Editor -> String -> Editor
appendRow e s = e { eNumRows = succ $ eNumRows e
                  , eRows = eRows e ++ [updateRender $ newERow s]
                  }

setMessageBar :: Editor -> MessageBar -> Editor
setMessageBar e mb = e { eMessageBar = mb }

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
