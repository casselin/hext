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
updateRender l = l { lineRenderSize = n
                   , lineRender = s
                   }
    where
        s = removeTabs . lineContents $ l
        n = length s

insertCharAt :: EditorLine -> Int -> Char -> EditorLine
insertCharAt l i c = updateRender . newELine $ xs ++ [c] ++ ys
    where (xs, ys) = splitAt i . lineContents $ l

deleteCharAt :: EditorLine -> Int -> EditorLine
deleteCharAt l i = updateRender . newELine $ xs ++ (drop 1 ys)
    where (xs, ys) = splitAt i . lineContents $ l

appendString :: EditorLine -> String -> EditorLine
appendString l s = updateRender . newELine $ (lineContents l) ++ s

lineAppend :: EditorLine -> EditorLine -> EditorLine
lineAppend l1 l2 = appendString l1 (lineContents l2)

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
