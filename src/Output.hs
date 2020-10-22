module Output where

import Editor.Types
import Editor.RowOps
import Terminal.IO
import Terminal.EscapeSequences


refreshScreen :: Editor -> IO ()
refreshScreen e =
    writeString $
        hideCursor                         ++
        setCursorPosition 1 1              ++
        drawRows e                         ++
        setCursorPosition
            ((y - rOff) + 1)
            ((x - cOff) + 1)               ++
        showCursor
    where
        x = ecx e
        y = ecy e
        rOff = eRowOffset e
        cOff = eColOffset e

emptyRows :: Rows -> String
emptyRows n = (concat . take n) $ repeat emptyline
    where
        emptyline = "~" ++ clearLine ++ "\r\n"

welcomeRow :: Cols -> String
welcomeRow c = "~" ++ (drop 1 padding) ++ welcome ++ padding
    where
        welcome = take c "Hext -- version 0.1.0.0"
        padlen  = (c - length welcome) `div` 2
        padding = take padlen $ cycle " "

drawWelcome :: Rows -> Cols -> [String]
drawWelcome r c =
    (take (k-1) $ repeat "~") ++
    [welcomeRow c]            ++
    (take (r-k) $ repeat "~")
    where
        k = r `div` 3

drawFile :: Editor -> [String]
drawFile e = map ( take c
                 . drop cOff
                 . rowRender
                 )
                 . take r
                 . drop rOff
                 $ xs
    where
        xs   = eRows e ++ repeat (newERow "~")
        r    = eScreenRows e
        rOff = eRowOffset e
        c    = eScreenCols e
        cOff = eColOffset e

mapDiffLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapDiffLast _ _ []     = []
mapDiffLast _ g [x]    = [g x]
mapDiffLast f g (x:xs) = f x : mapDiffLast f g xs

drawRows :: Editor -> String
drawRows e =
    if n == 0
    then concat . mapDiffLast (++ newLine) (++ clearLine) $
             drawWelcome r c
    else concat . mapDiffLast (++ newLine) (++ clearLine) $
             (drawFile e)
    where
        n = eNumRows e
        r = eScreenRows e
        c = eScreenCols e
