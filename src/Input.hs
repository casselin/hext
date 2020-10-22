module Input where

import Data.Bits ((.&.))
import Data.Char (ord, chr)

import Editor.Types


ctrlkey :: Char -> Char
ctrlkey = let bitmask = 0x1f
          in chr . (.&.) bitmask . ord

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
horiScroll e = if x < cOff
               then e { eColOffset = x }
               else if x >= cOff + c
                    then e { eColOffset = x - c + 1 }
                    else e
    where
        x = ecx e
        c = eScreenCols e
        cOff = eColOffset e

scroll :: Editor -> Editor
scroll = horiScroll . vertScroll

moveCursor :: Editor -> Direction -> Editor
moveCursor e d = case d of
    ArrowUp    -> e { ecy = if y > 0 then (y - 1) else y }
    ArrowDown  -> e { ecy = if y < n then (y + 1) else y }
    ArrowLeft  -> if x > 0
                  then e { ecx = (x - 1) }
                  else if y > 0
                       then e { ecy = (y - 1)
                              , ecx = rowSize $ (eRows e) !! (y - 1)
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
    PageUp     -> foldl moveCursor e . take r $ repeat ArrowUp
    PageDown   -> foldl moveCursor e . take r $ repeat ArrowDown
    HomeKey    -> e { ecx = 0 }
    EndKey     -> e { ecx = c - 1 }
    where x = ecx e
          y = ecy e
          r = eScreenRows e
          c = eScreenCols e
          n = eNumRows e
          l = if y < n then rowSize $ (eRows e) !! y else 0

snapCursor :: Editor -> Editor
snapCursor e = e { ecx = if x > l then l else x }
    where x = ecx e
          y = ecy e
          n = eNumRows e
          l = if y < n then rowSize $ (eRows e) !! y else 0

updateCursor :: Direction -> Editor -> Editor
updateCursor d = scroll . snapCursor . flip moveCursor d

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
        _    -> Letter '\ESC'
parseKey (Right c)
    | ord c >= 1 || ord c <= 26 = Ctrl (unctrlkey c)
    | otherwise = Letter c

processKey :: Editor -> KeyPress -> Maybe Editor
processKey e (Letter _) = Just e
processKey e (Ctrl c)   = if c == 'q' then Nothing else Just e
processKey e (Escape d) = Just $ updateCursor d e
