import System.Posix.Terminal
import System.Posix.IO (fdRead, stdInput)
import System.Posix.Types (ByteCount)

import System.Exit
import Control.Exception (finally, catch, IOException)

import Data.Bits ((.&.))
import Data.Char (isControl, ord, chr)

ctrlkey :: Char -> Char
ctrlkey = let bitmask = 0x1f
          in chr . (.&.) bitmask . ord

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

readKey :: IO Char
readKey = let handler :: IOException -> IO (String, ByteCount)
              handler e = return ("\0", 1)
              in do
                  (c:_, _) <- (fdRead stdInput 1) `catch` handler
                  return c

mainloop :: IO ()
mainloop = do
    c <- readKey
    let d = ord c
    if isControl c
        then putStr $ show d ++ "\r\n"
        else putStr $ show d ++ " (" ++ show c ++ ")\r\n"
    if c == ctrlkey 'q'
        then return ()
        else mainloop

main :: IO ()
main = withRawInput mainloop
