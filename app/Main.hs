module Main where

import Terminal
import Input

import Data.Bits ((.&.))
import Data.Char (isControl, ord, chr)

mainloop :: IO ()
mainloop = do
    c <- readKey
    processKey c
    mainloop

main :: IO ()
main = withRawInput mainloop
