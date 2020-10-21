module Editor.Types where


type Rows = Int
type Cols = Int

data Editor = Editor
    { ecx :: Cols
    , ecy :: Rows
    , eColOffset :: Cols
    , eRowOffset :: Rows
    , eScreenCols :: Cols
    , eScreenRows :: Rows
    , eNumLines :: Rows
    , eLines :: [EditorLine]
    }

newEditor :: Editor
newEditor = Editor
    { ecx = 1
    , ecy = 1
    , eColOffset = 1
    , eRowOffset = 1
    , eScreenCols = 0
    , eScreenRows = 0
    , eNumLines = 0
    , eLines = []
    }

data EditorLine = EditorLine
    { lineSize :: Int
    , lineContents :: String
    }

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
