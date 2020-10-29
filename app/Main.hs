module Main where

import Data.Time.Clock.System (getSystemTime)
import System.Environment (getArgs)

import Editor.Types
import Terminal.IO
import Input
import Output
import FileIO


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
    return $ (editorOpen e file) { eFileName = s }

mainloop :: Editor -> IO ()
mainloop e = do
    t <- getSystemTime
    refreshScreen e t
    result <- (processKey e . parseKey) <$> readKey
    case result of
        Just e' -> mainloop e'
        Nothing -> exitProgram

main :: IO ()
main = withRawInput $ getArgs >>= initEditor >>= mainloop
