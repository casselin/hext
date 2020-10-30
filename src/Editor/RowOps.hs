module Editor.RowOps where

import Editor.Types


appendRow :: Editor -> String -> Editor
appendRow e s = e { eNumRows = succ $ eNumRows e
                  , eRows = eRows e ++ [updateRender $ newERow s]
                  }

newERow :: String -> EditorRow
newERow s = updateRender $ EditorRow
    { rowSize = length s
    , rowRenderSize = 0
    , rowContents = s
    , rowRender = ""
    }

insertCharAt :: String -> Int -> Char -> String
insertCharAt s i c = xs ++ [c] ++ ys
    where (xs, ys) = splitAt i s

rowInsertChar :: EditorRow -> Int -> Char -> EditorRow
rowInsertChar er i c = undefined

nextTab :: Cols -> Cols
nextTab n = tabStop - (n `mod` tabStop)

ecxToErx :: String -> Cols -> Cols
ecxToErx s x = go s x 0
    where
        go :: String -> Cols -> Cols -> Cols
        go []        _ r = r
        go  _        0 r = r
        go ('\t':cs) i r = go cs (i - 1) (r + nextTab r)
        go (_:cs)    i r = go cs (i - 1) (r + 1)

removeTabs :: String -> String
removeTabs s = go 0 s
    where
        go :: Cols -> String -> String
        go _ []        = []
        go i ('\t':cs) = replicate (nextTab i) ' ' ++ go (i + (nextTab i)) cs
        go i (c:cs)    = c : go (i + 1) cs

updateRender :: EditorRow -> EditorRow
updateRender er = er { rowRenderSize = n
                     , rowRender = s }
    where
        s = removeTabs . rowContents $ er
        n = length s
