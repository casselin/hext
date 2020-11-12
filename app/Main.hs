module Main where

import Data.Time.Clock.System (getSystemTime)
import System.Environment (getArgs)

import Terminal.IO
import Editor.Editor
import Editor.Input
import Editor.Output
import Editor.IO


mainloop :: Editor -> IO ()
mainloop e = do
    t <- getSystemTime
    writeString $ refreshScreen e t
    (req, e') <- (processKey e . parseKey) <$> readKey
    case req of
        Empty -> mainloop e'
        Exit  -> exitProgram

main :: IO ()
main = withRawInput $ getArgs >>= initEditor >>= mainloop
