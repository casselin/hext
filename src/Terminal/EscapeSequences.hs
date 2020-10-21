module Terminal.EscapeSequences where


hideCursor :: String
hideCursor = "\ESC[?25l"

showCursor :: String
showCursor = "\ESC[?25h"

bottomRightCursor :: String
bottomRightCursor = "\ESC[999C\ESC[999B"

reportCursorPosition :: String
reportCursorPosition = "\ESC[6n"

clearLine :: String
clearLine = "\ESC[K"

newLine :: String
newLine = "\ESC[K\r\n"

clearScreen :: String
clearScreen = "\ESC[2J\ESC[H"
