module Editor.File where

import qualified Data.Sequence as Seq (empty)
import Editor.Editor
import Editor.Line


clearFile :: Editor -> Editor
clearFile e = e { eLines    = Seq.empty
                , eFileName = ""
                }

setFileName :: Editor -> FilePath -> Editor
setFileName e fp = e { eFileName = fp }

loadContents :: Editor -> String -> Editor
loadContents e = (foldl appendLine e) . lines

loadFile :: Editor -> FilePath -> String -> Editor
loadFile e fp s = flip loadContents s
                $ flip setFileName fp
                $ clearFile e

linesToString :: Editor -> String
linesToString = foldr ((\a b -> a ++ "\n" ++ b) . lineContents) "" . eLines
