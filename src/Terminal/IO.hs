module Terminal.IO where

import System.Posix.Terminal
import System.Posix.Types (ByteCount)
import System.Posix.IO
    ( fdRead
    , stdInput
    , fdWrite
    , stdOutput
    )
import System.Exit (exitSuccess, die)

import Data.List (stripPrefix)
import Data.Char (isDigit)

import Control.Exception (finally, catch, IOException)

import Editor.Types
import Terminal.EscapeSequences


readCursorPosition :: IO String
readCursorPosition = go ""
    where go cs = do
              c <- readByte
              if c == '\0'
                  then return cs
                  else go (cs ++ [c])

getCursorPosition :: String -> Maybe (Rows, Cols)
getCursorPosition s = do
    coord <- stripPrefix "\ESC[" s
    let (rs, cs) = fmap (filter (isDigit)) . break (== ';') $ coord
    return (read rs, read cs)

setCursorPosition :: Rows -> Cols -> String
setCursorPosition r c = "\ESC[" ++
                        show r  ++
                        ";"     ++
                        show c  ++
                        "H"

getWindowSize :: IO (Either String (Rows, Cols))
getWindowSize = do
    _ <- fdWrite stdOutput bottomRightCursor
    _ <- fdWrite stdOutput reportCursorPosition
    escSeq <- readCursorPosition
    case getCursorPosition escSeq of
        Just x -> return (Right x)
        Nothing -> return (Left "Error: getWindowSize")

readByte :: IO Char
readByte = let handler :: IOException -> IO (String, ByteCount)
               handler _ = return ("\0", 1)
               in do
                   (c:_, _) <- (fdRead stdInput 1) `catch` handler
                   return c

readEscSeq :: IO String
readEscSeq = do
    (s, _) <- (fdRead stdInput 3) `catch` handler
    return s
    where
        handler :: IOException -> IO (String, ByteCount)
        handler _ = return ("", 0)

readKey :: IO (Either EscSeq Char)
readKey = do
    c <- readByte
    case c of
        '\ESC' -> do
            cs <- readEscSeq
            return (Left $ EscSeq (c : cs))
        _ -> return (Right c)

writeString :: String -> IO ()
writeString s = fdWrite stdOutput s >> return ()


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
    --hSetBuffering stdout NoBuffering
    oldTermSettings <- getTerminalAttributes stdInput
    let rawSettings = rawMode oldTermSettings
    setTerminalAttributes stdInput rawSettings WhenFlushed
    app `finally` disableRaw oldTermSettings

disableRaw :: TerminalAttributes -> IO ()
disableRaw orig = do
    --hSetBuffering stdout LineBuffering
    setTerminalAttributes stdInput orig Immediately

exitDie :: String -> IO ()
exitDie s = writeString clearScreen >> die s

exitProgram :: IO ()
exitProgram = writeString clearScreen >> exitSuccess
