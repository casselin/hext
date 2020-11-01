module Editor.FileIO where

import Editor.Types
import Editor.RowOps


editorOpen :: Editor -> String -> Editor
editorOpen e s = (foldl appendRow e) . lines $ s
