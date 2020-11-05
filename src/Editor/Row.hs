module Editor.Row where


data EditorRow = EditorRow
    { rowSize       :: Int
    , rowRenderSize :: Int
    , rowContents   :: String
    , rowRender     :: String
    }
    deriving (Show, Eq)

newERow :: String -> EditorRow
newERow s = updateRender $ EditorRow
    { rowSize = length s
    , rowRenderSize = 0
    , rowContents = s
    , rowRender = ""
    }

updateRender :: EditorRow -> EditorRow
updateRender er = er { rowRenderSize = n
                     , rowRender = s }
    where
        s = removeTabs . rowContents $ er
        n = length s

rowInsertChar :: EditorRow -> Int -> Char -> EditorRow
rowInsertChar er i c = updateRender . newERow $ xs ++ [c] ++ ys
    where (xs, ys) = splitAt i . rowContents $ er

tabStop :: Int
tabStop = 8

nextTab :: Int -> Int
nextTab n = tabStop - (n `mod` tabStop)

ecxToErx :: EditorRow -> Int -> Int
ecxToErx er x = go s x 0
    where
        s = rowContents er
        go :: String -> Int -> Int -> Int
        go []        _ r = r
        go  _        0 r = r
        go ('\t':cs) i r = go cs (i - 1) (r + nextTab r)
        go (_:cs)    i r = go cs (i - 1) (r + 1)

removeTabs :: String -> String
removeTabs s = go 0 s
    where
        go :: Int -> String -> String
        go _ []        = []
        go i ('\t':cs) = replicate (nextTab i) ' ' ++ go (i + (nextTab i)) cs
        go i (c:cs)    = c : go (i + 1) cs
