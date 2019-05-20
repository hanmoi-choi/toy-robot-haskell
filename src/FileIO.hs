module FileIO where

processTextFile :: FilePath -> IO String
processTextFile fname = do
  readFile fname
