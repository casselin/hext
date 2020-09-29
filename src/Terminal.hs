module Terminal
    ( withRawInput
    , readKey
    ) where

import System.Posix.Terminal
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Types (ByteCount)

import Control.Exception (finally, catch, IOException)

readKey :: IO Char
readKey = let handler :: IOException -> IO (String, ByteCount)
              handler e = return ("\0", 1)
              in do
                  (c:_, _) <- (fdRead stdInput 1) `catch` handler
                  return c

rawMode :: TerminalAttributes -> TerminalAttributes
rawMode =
    flip (foldl withoutMode)
      [ EnableEcho              -- ECHO
      , ProcessInput            -- ICANON
      , KeyboardInterrupts      -- ISIG
      , StartStopOutput         -- IXON
      , ExtendedFunctions       -- IEXTEN
      , MapCRtoLF               -- ICRNL
      , ProcessOutput           -- OPOST
      , InterruptOnBreak        -- BRKINT
      , CheckParity             -- INPCK
      , StripHighBit            -- ISTRIP
      ]                   .
      flip withBits     8 .     -- CS8
      flip withMinInput 0 .     -- VMIN
      flip withTime     1       -- VTIME

withRawInput :: IO a -> IO a
withRawInput app = do
    oldTermSettings <- getTerminalAttributes stdInput
    let rawSettings = rawMode oldTermSettings
    setTerminalAttributes stdInput rawSettings WhenFlushed
    app `finally` setTerminalAttributes stdInput oldTermSettings Immediately
