
module Utils where

import           System.IO (hPutStrLn, stderr)

is :: Bool -> Bool
is = id

report :: String -> IO ()
report = hPutStrLn stderr
