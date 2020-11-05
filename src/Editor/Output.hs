module Editor.Output where

import Data.Time.Clock.System (SystemTime(systemSeconds))

import Editor.Types
import Terminal.IO
import Terminal.EscapeSequences


refreshScreen :: Editor -> SystemTime -> String
refreshScreen e t =
    hideCursor                         ++
    setCursorPosition 1 1              ++
    drawRows e                         ++
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

drawRows :: Editor -> String
drawRows e =
    if n == 0
    then concat . map (++ newLine) $ drawWelcome r c
    else concat . map (++ newLine) $ (drawFile e)
    where
        n = eNumRows e
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
        status = filename ++ " - " ++ (show $ eNumRows e) ++ " lines"
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
        start = mbTime . eMessageBar $ e
        inTime = systemSeconds t - systemSeconds start < 5
        msg = if inTime
              then take c . mbContents . eMessageBar $ e
              else ""
