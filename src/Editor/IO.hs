module Editor.IO where

import Data.Time.Clock.System (getSystemTime)

import Terminal.IO
import Editor.Editor
import Editor.File


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
