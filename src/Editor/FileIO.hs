module Editor.FileIO where

import Editor.Types
import Editor.RowOps


loadFile :: Editor -> FilePath -> String -> Editor
loadFile e fp s = (foldl appendRow e') . lines $ s
    where e' = e { eFileName = fp }
