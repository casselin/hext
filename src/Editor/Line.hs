module Editor.Line where


data EditorLine = EditorLine
    { lineSize       :: Int
    , lineRenderSize :: Int
    , lineContents   :: String
    , lineRender     :: String
    }
    deriving (Show, Eq)

newELine :: String -> EditorLine
newELine s = updateRender $ EditorLine
    { lineSize = length s
    , lineRenderSize = 0
    , lineContents = s
    , lineRender = ""
    }

updateRender :: EditorLine -> EditorLine
updateRender el = el { lineRenderSize = n
                     , lineRender = s }
    where
        s = removeTabs . lineContents $ el
        n = length s

lineInsertChar :: EditorLine -> Int -> Char -> EditorLine
lineInsertChar el i c = updateRender . newELine $ xs ++ [c] ++ ys
    where (xs, ys) = splitAt i . lineContents $ el

tabStop :: Int
tabStop = 8

nextTab :: Int -> Int
nextTab n = tabStop - (n `mod` tabStop)

ecxToErx :: EditorLine -> Int -> Int
ecxToErx el x = go s x 0
    where
        s = lineContents el
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
