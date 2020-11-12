module Editor.IO where

import Data.Time.Clock.System (getSystemTime)

import Terminal.IO
import Editor.Editor
import Editor.File
import Editor.Input


handleIORequest :: Editor -> IORequest -> IO Editor
handleIORequest e None = return e
handleIORequest e Save = do
    saveFile e
    return e
handleIORequest e Exit = exitProgram >> return e

initEditor :: [String] -> IO Editor
initEditor as = initWindow newEditor >>=
                initMessageBar       >>=
                case as of
                    []    -> return
                    (a:_) -> initFile a

initMessageBar :: Editor -> IO Editor
initMessageBar e = do
    t <- getSystemTime
    return $ setMessageBar e "HELP: Ctrl-Q = quit" t

initWindow :: Editor -> IO Editor
initWindow e = do
    window <- getWindowSize
    case window of
        Left s -> error s
        Right (r,c) -> return e { eScreenRows = r - 2
                                , eScreenCols = c }

initFile :: String -> Editor -> IO Editor
initFile s e = do
    file <- readFile s
    return $ loadFile e s file

saveFile :: Editor -> IO ()
saveFile e = if eFileName e == ""
             then return ()
             else writeFile (eFileName e) (linesToString e)
