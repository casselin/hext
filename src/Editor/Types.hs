module Editor.Types where


type Rows = Int
type Cols = Int

data Editor = Editor
    { ecx         :: Cols
    , ecy         :: Rows
    , erx         :: Cols
    , eColOffset  :: Cols
    , eRowOffset  :: Rows
    , eScreenCols :: Cols
    , eScreenRows :: Rows
    , eNumRows    :: Rows
    , eRows       :: [EditorRow]
    }

newEditor :: Editor
newEditor = Editor
    { ecx         = 0
    , ecy         = 0
    , erx         = 0
    , eColOffset  = 0
    , eRowOffset  = 0
    , eScreenCols = 0
    , eScreenRows = 0
    , eNumRows    = 0
    , eRows       = []
    }

data EditorRow = EditorRow
    { rowSize       :: Int
    , rowRenderSize :: Int
    , rowContents   :: String
    , rowRender     :: String
    }
    deriving (Show, Eq)

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
