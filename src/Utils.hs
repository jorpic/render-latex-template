
module Utils where

import System.IO (hPutStrLn, stderr)

report :: String -> IO ()
report = hPutStrLn stderr
