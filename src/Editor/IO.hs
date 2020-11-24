module Editor.IO where

import Data.Time.Clock.System (getSystemTime)
import System.IO.Error (ioeGetErrorString)
import Control.Exception (try, IOException)

import Terminal.IO
import Editor.Editor
import Editor.File
import Editor.Input
import Editor.Output


handleIORequest :: (IORequest, Editor) -> IO Editor
handleIORequest (Ignore, e) = return e
handleIORequest (None, e)   = return . unQuitConfirm $ e
handleIORequest (Save, e)   = (saveFile . unQuitConfirm $ e) >>= return
handleIORequest (Exit, e)   = handleExit e

handleExit :: Editor -> IO Editor
handleExit e
    | eQuitConfirm e = exitProgram >> return e
    | eDirty e = do
        let e' = setMessageBar e "Unsaved changes: Press Ctrl-Q again to quit"
        return $ e' { eQuitConfirm = True }
    | otherwise = exitProgram >> return e

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
    return $ (loadFile e s file) { eDirty = False }

saveFile :: Editor -> IO Editor
saveFile e = case file of
    "" -> do
        (filename, e') <- promptInput "Save as: " e ""
        case filename of
            Nothing -> return . setMessageBar e' $ "Save aborted"
            Just fp -> saveFile $ e' { eFileName = fp }
    _  -> do
        result <- try (writeFile file s) :: IO (Either IOException ())
        case result of
            (Left err) -> do
                let e' = setMessageBar e $ "Save failed: " ++
                                           ioeGetErrorString err
                return e' { eFileName = "" }
            (Right _) -> do
                let e' = setMessageBar e $ "Saved " ++ file
                return $ e' { eDirty = False }
    where
        file = eFileName e
        s = linesToString e

promptInput :: String -> Editor -> String -> IO (Maybe String, Editor)
promptInput p e s = do
    t <- getSystemTime
    writeString . refreshScreen $ setMessageBar e
        (p ++ s ++ " (ESC to cancel)")
    input <- parseKey <$> readKey
    case input of
        Enter -> if null s
                     then promptInput p e s
                     else return (Just s, e { eTime = t })
        Esc -> return (Nothing, e { eTime = t })
        Backspace -> if null s
                         then promptInput p e s
                         else promptInput p e (init s)
        Letter c -> promptInput p e (s ++ [c])
        _ -> promptInput p e s
