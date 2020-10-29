module Output where

import Data.Time.Clock.System
    ( SystemTime(systemSeconds)
    , getSystemTime
    )

import Editor.Types
import Editor.RowOps
import Terminal.IO
import Terminal.EscapeSequences

-- updateScreen :: Editor -> IO ()
-- updateScreen e = do
--     let rx = erx e
--     let y = ecy e
--     let rOff = eRowOffset e
--     let cOff = eColOffset e
--     let screen = hideCursor                         ++
--                  setCursorPosition 1 1              ++
--                  drawRows e                         ++
--                  drawStatusBar e
--     messageBar <- drawMessageBar e
--     writeString $ screen                ++
--                   messageBar            ++
--                   setCursorPosition
--                       ((y - rOff) + 1)
--                       ((rx - cOff) + 1) ++
--                   showCursor


refreshScreen :: Editor -> IO ()
refreshScreen e =
    writeString $
        hideCursor                         ++
        setCursorPosition 1 1              ++
        drawRows e                         ++
        drawStatusBar e                    ++
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

timelimit :: SystemTime -> IO Bool
timelimit t = do
    now <- getSystemTime
    return $ systemSeconds now - systemSeconds t < 5

drawMessageBar :: Editor -> IO String
drawMessageBar e = do
    let statusMsg = smContents . eStatusMsg $ e
    let r = eScreenRows e
    let c = eScreenCols e
    start <- smTime . eStatusMsg $ e
    inTime <- timelimit start
    if inTime
        then return $ setCursorPosition (r+2) 1 ++
                      clearLine                 ++
                      take c statusMsg
        else return $ setCursorPosition (r+2) 1 ++ clearLine
