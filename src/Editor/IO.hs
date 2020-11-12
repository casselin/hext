module Editor.IO where

import Data.Time.Clock.System (getSystemTime)
import Control.Exception (try, IOException)

import Terminal.IO
import Editor.Editor
import Editor.File
import Editor.Input


handleIORequest :: (IORequest, Editor) -> IO Editor
handleIORequest (None, e) = return e
handleIORequest (Save, e) = saveFile e >>= return
handleIORequest (Exit, e) = exitProgram >> return e

initEditor :: [String] -> IO Editor
initEditor as = initWindow newEditor >>=
                initMessageBar       >>=
                case as of
                    []    -> return
                    (a:_) -> initFile a

initMessageBar :: Editor -> IO Editor
initMessageBar e = do
    t <- getSystemTime
    return $ setMessageBar e { eTime = t }
        "HELP: Ctrl-S = save | Ctrl-Q = quit"

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

saveFile :: Editor -> IO Editor
saveFile e = case file of
    "" -> return e
    _  -> do
        result <- try (writeFile file s) :: IO (Either IOException ())
        case result of
            (Left err) ->
                return . setMessageBar e $ "Save failed: " ++ show err
            (Right _) -> return . setMessageBar e $ "Saved " ++ file
    where
        file = eFileName e
        s = linesToString e
