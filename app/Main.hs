module Main where

import Data.Time.Clock.System (getSystemTime)
import System.Environment (getArgs)

import Terminal.IO
import Editor.Editor
import Editor.Input
import Editor.Output
import Editor.FileIO


initEditor :: [String] -> IO Editor
initEditor as = initWindow newEditor >>=
                initMessageBar       >>=
                initFile as

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

initFile :: [String] -> Editor -> IO Editor
initFile [] e = return e
initFile (s:_) e = do
    file <- readFile s
    return $ loadFile e s file

mainloop :: Editor -> IO ()
mainloop e = do
    t <- getSystemTime
    writeString $ refreshScreen e t
    result <- (processKey e . parseKey) <$> readKey
    case result of
        Just e' -> mainloop e'
        Nothing -> exitProgram

main :: IO ()
main = withRawInput $ getArgs >>= initEditor >>= mainloop
