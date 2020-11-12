module Editor.Input where

import qualified Data.Sequence as Seq
    ( index
    , update
    )
import Data.Char (ord, chr)

import Terminal.IO
import Editor.Editor
import Editor.Line

data IORequest
    = None
    | Save
    | Exit

data KeyPress
    = Null
    | Enter
    | Esc
    | Backspace
    | Letter Char
    | Ctrl Char
    | Escape Direction
    deriving (Show, Eq)

data Direction
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | DelKey
    | PageUp
    | PageDown
    | HomeKey
    | EndKey
    deriving (Show, Eq)

unctrlkey :: Char -> Char
unctrlkey = chr . (+ 96) . ord

vertScroll :: Editor -> Editor
vertScroll e = if y < rOff
               then e { eRowOffset = y }
               else if y >= rOff + r
                    then e { eRowOffset = y - r + 1 }
                    else e
    where
        y = ecy e
        r = eScreenRows e
        rOff = eRowOffset e

horiScroll :: Editor -> Editor
horiScroll e = if rx < cOff
               then e { eColOffset = rx }
               else if rx >= cOff + c
                    then e { eColOffset = rx - c + 1 }
                    else e
    where
        rx = erx e
        c = eScreenCols e
        cOff = eColOffset e

updateErx :: Editor -> Editor
updateErx e = e { erx = ecxToErx er x }
    where x = ecx e
          y = ecy e
          n = eNumLines e
          er = if y < n then (eLines e) `Seq.index` y else newELine ""

scroll :: Editor -> Editor
scroll = horiScroll . vertScroll . updateErx

moveCursor :: Editor -> Direction -> Editor
moveCursor e d = case d of
    ArrowUp    -> e { ecy = if y > 0 then (y - 1) else y }
    ArrowDown  -> e { ecy = if y < n then (y + 1) else y }
    ArrowLeft  -> if x > 0
                  then e { ecx = (x - 1) }
                  else if y > 0
                       then e { ecy = (y - 1)
                              , ecx =
                                  lineSize $ (eLines e) `Seq.index` (y - 1)
                              }
                       else e
    ArrowRight -> if x < l
                  then e { ecx = (x + 1) }
                  else if y < n
                       then e { ecy = (y + 1)
                              , ecx = 0
                              }
                       else e
    DelKey     -> e
    PageUp     -> foldl moveCursor (e { ecy = rOff }) .
                      take r $ repeat ArrowUp
    PageDown   -> foldl moveCursor (e { ecy = min (rOff + r - 1) n }).
                      take r $ repeat ArrowDown
    HomeKey    -> e { ecx = 0 }
    EndKey     -> e { ecx = l }
    where x = ecx e
          y = ecy e
          r = eScreenRows e
          rOff = eRowOffset e
          n = eNumLines e
          l = if y < n then lineSize $ (eLines e) `Seq.index` y else 0

snapCursor :: Editor -> Editor
snapCursor e = e { ecx = if x > l then l else x }
    where x = ecx e
          y = ecy e
          n = eNumLines e
          l = if y < n then lineSize $ (eLines e) `Seq.index` y else 0

updateCursor :: Direction -> Editor -> Editor
updateCursor d = scroll . snapCursor . flip moveCursor d

insertChar :: Editor -> Char -> Editor
insertChar e c = updateCursor ArrowRight
    e' { eLines = Seq.update y (lineInsertCharAt el x c) (eLines e') }
    where
        x = ecx e
        y = ecy e
        n = eNumLines e
        e' = if y == n then appendLine e "" else e
        el = (eLines e') `Seq.index` y

parseKey :: Either EscSeq Char -> KeyPress
parseKey (Left (EscSeq s)) =
    case (drop 2 s) of
        "1~" -> Escape HomeKey
        "3~" -> Escape DelKey
        "4~" -> Escape EndKey
        "5~" -> Escape PageUp
        "6~" -> Escape PageDown
        "7~" -> Escape HomeKey
        "8~" -> Escape EndKey
        "A"  -> Escape ArrowUp
        "B"  -> Escape ArrowDown
        "C"  -> Escape ArrowRight
        "D"  -> Escape ArrowLeft
        "H"  -> Escape HomeKey
        "OH" -> Escape HomeKey
        "F"  -> Escape EndKey
        "OF" -> Escape EndKey
        _    -> Esc
parseKey (Right c)
    | c == '\NUL'             = Null
    | c == '\r'               = Enter
    | c == '\DEL'             = Backspace
    | c >= '\1' && c <= '\26' = Ctrl (unctrlkey c)
    | otherwise               = Letter c

processKey :: Editor -> KeyPress -> (IORequest, Editor)
processKey e (Null)      = (None, e)
processKey e (Enter)     = (None, e) -- TODO
processKey e (Esc)       = (None, e)
processKey e (Backspace) = (None, e) -- TODO
processKey e (Letter c)  = (None, insertChar e $ c)
processKey e (Escape d)  = (None, updateCursor d e)
processKey e (Ctrl c)    = case c of
    'q' -> (Exit, e)
    's' -> (Save, e)
    _   -> (None, e)
