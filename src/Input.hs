module Input
    ( processKey
    ) where

import Data.Bits ((.&.))
import Data.Char (isControl, ord, chr)

import System.Exit (exitSuccess)

import Terminal (readKey)

ctrlkey :: Char -> Char
ctrlkey = let bitmask = 0x1f
          in chr . (.&.) bitmask . ord


processKey :: Char -> IO ()
processKey c
    | ctrlkey 'q' == c = exitSuccess
    | otherwise        = return ()
