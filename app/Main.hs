module Main where

import System.Environment (getArgs)

import Editor.Types
import Terminal.IO
import Input
import Output
import FileIO


initEditor :: [String] -> IO Editor
initEditor []    = initWindow newEditor
initEditor (s:_) = initWindow newEditor >>= initFile s

initWindow :: Editor -> IO Editor
initWindow e = do
    window <- getWindowSize
    case window of
        Left s -> error s
        Right (r,c) -> return e { eScreenRows = r
                                , eScreenCols = c }

initFile :: String -> Editor -> IO Editor
initFile s e = do
    file <- readFile s
    return $ editorOpen e file

mainloop :: Editor -> IO ()
mainloop e = do
    refreshScreen e
    result <- (processKey e . parseKey) <$> readKey
    case result of
        Just e' -> mainloop e'
        Nothing -> exitProgram

main :: IO ()
main = withRawInput $ getArgs >>= initEditor >>= mainloop
