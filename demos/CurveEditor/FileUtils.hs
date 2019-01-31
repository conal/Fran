-- Very simple file utilities

module FileUtils where

import IO


-- Load and save values

load :: Read a => String -> IO a
load fileName = do
  --putStrLn ("loading from " ++ show fileName)
  hFile <- openFile fileName ReadMode
  str <- hGetContents hFile
  return (read str)
  -- Note: no hClose.  It will happen automatically on eof or gc.

save :: Show a => String -> a -> IO ()
save fileName val = do
  --putStrLn ("saving to " ++ show fileName)
  hFile <- openFile fileName WriteMode
  hPutStr hFile (show val)
  hClose hFile
