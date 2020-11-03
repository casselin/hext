module Editor.Types where

import Data.Time.Clock.System (SystemTime(MkSystemTime))

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
    , eMessageBar    = newMessageBar
    }

data EditorRow = EditorRow
    { rowSize       :: Int
    , rowRenderSize :: Int
    , rowContents   :: String
    , rowRender     :: String
    }
    deriving (Show, Eq)

newERow :: String -> EditorRow
newERow s = EditorRow
    { rowSize = length s
    , rowRenderSize = 0
    , rowContents = s
    , rowRender = ""
    }

data MessageBar = MessageBar
    { mbContents :: String
    , mbTime     :: SystemTime
    }
    deriving (Show, Eq)

newMessageBar :: MessageBar
newMessageBar = MessageBar
    { mbContents = ""
    , mbTime     = MkSystemTime 0 0}

setMessageBar :: Editor -> String -> SystemTime -> Editor
setMessageBar e s t = e { eMessageBar = MessageBar s t }

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

tabStop :: Int
tabStop = 8
