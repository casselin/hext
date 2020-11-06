module Editor.Output where

import Data.Time.Clock.System (SystemTime)

import Terminal.IO
import Terminal.EscapeSequences
import Editor.Editor
import Editor.Line
import Editor.MessageBar


refreshScreen :: Editor -> SystemTime -> String
refreshScreen e t =
    hideCursor                         ++
    setCursorPosition 1 1              ++
    drawLines e                         ++
    drawStatusBar e                    ++
    drawMessageBar e t                 ++
    setCursorPosition
        ((y - rOff) + 1)
        ((rx - cOff) + 1)              ++
    showCursor
    where
        rx = erx e
        y = ecy e
        rOff = eRowOffset e
        cOff = eColOffset e

welcomeLine :: Cols -> String
welcomeLine c = "~" ++ (drop 1 padding) ++ welcome ++ padding
    where
        welcome = take c "Hext -- version 0.1.0.0"
        padlen  = (c - length welcome) `div` 2
        padding = take padlen $ cycle " "

drawWelcome :: Rows -> Cols -> [String]
drawWelcome r c =
    (take (k-1) $ repeat "~") ++
    [welcomeLine c]            ++
    (take (r-k) $ repeat "~")
    where
        k = r `div` 3

drawFile :: Editor -> [String]
drawFile e = map ( take c
                 . drop cOff
                 . lineRender
                 )
                 . take r
                 . drop rOff
                 $ xs
    where
        xs   = eLines e ++ repeat (newELine "~")
        r    = eScreenRows e
        rOff = eRowOffset e
        c    = eScreenCols e
        cOff = eColOffset e

drawLines :: Editor -> String
drawLines e =
    if n == 0
    then concat . map (++ newLine) $ drawWelcome r c
    else concat . map (++ newLine) $ (drawFile e)
    where
        n = eNumLines e
        r = eScreenRows e
        c = eScreenCols e

drawStatusBar :: Editor -> String
drawStatusBar e = setCursorPosition (r+1) 1 ++
                  negativeColour            ++
                  status                    ++
                  replicate padding ' '     ++
                  pos                       ++
                  defaultColour
    where
        r = eScreenRows e
        c = eScreenCols e
        filename = case eFileName e of
            "" -> "untitled"
            s  -> s
        status = filename ++ " - " ++ (show $ eNumLines e) ++ " lines"
        lstatus = length status
        pos = (show . (+ 1) . ecy) e ++ ":" ++ (show . (+ 1) . ecx) e
        lpos = length pos
        padding = c - lstatus - lpos

drawMessageBar :: Editor -> SystemTime -> String
drawMessageBar e t = setCursorPosition (r+2) 1 ++
                     clearLine                 ++
                     msg
    where
        r = eScreenRows e
        c = eScreenCols e
        mb = eMessageBar e
        msg = if inTime mb t
              then take c . mbContents $ mb
              else ""
