module Editor.RowOps where

import Editor.Types


appendRow :: Editor -> String -> Editor
appendRow e l = e { eNumLines = succ $ eNumLines e
                  , eLines = eLines e ++ [EditorLine (length l) l]
                  }
