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
    let e' = e { eTime = t }
    writeString . refreshScreen $ e'
    result <- (processKey e' . parseKey) <$> readKey
    handleIORequest result >>= mainloop

main :: IO ()
main = withRawInput $ getArgs >>= initEditor >>= mainloop
