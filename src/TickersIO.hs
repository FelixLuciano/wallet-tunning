module TickersIO
    ( fromFileIO
    ) where

import Tickers
    ( Tickers
    )

fromFileIO :: FilePath -> IO Tickers
fromFileIO filePath = do
    content <- readFile filePath
    return $ lines content
