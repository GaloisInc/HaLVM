-- |This defines the interface by which applications can use the console
-- provided by whatever underlying system is being used.
module HaLVM.Interface.Console(
         Console(..)
       )
 where

data Console = Console {
       -- |Initializes the console. After running this function, the
       -- various standard console functions (`putStr`, for example, or
       -- `hPutStr` stdout) will utilize this console. (Before this call,
       -- they may go anywhere.)
       initializeConsole :: IO ()
     , consoleRead       :: IO String
     , consoleWrite      :: String -> IO ()
     }

