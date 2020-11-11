module Editor.Editor where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq (empty)
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
    , eLines         :: Seq EditorLine
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
    , eLines         = Seq.empty
    , eFileName      = ""
    , eMessageBar    = emptyMessageBar
    }

eNumLines :: Editor -> Int
eNumLines = length . eLines

appendLine :: Editor -> String -> Editor
appendLine e s =
    e { eLines = eLines e |> (updateRender $ newELine s) }

setMessageBar :: Editor -> String -> SystemTime -> Editor
setMessageBar e s t =
    e { eMessageBar = newMessageBar s t }
