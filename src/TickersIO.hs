module TickersIO
    ( readFileTickers
    , readFilePricesHistory
    ) where

import Tickers
    ( Tickers
    , StockPrice
    , StockPricesHistory
    , newStockPricesHistory
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    )

readFileTickers :: FilePath -> IO Tickers
readFileTickers filePath = do
    content <- readFile filePath
    return $ lines content

readFilePriceList :: FilePath -> IO [StockPrice]
readFilePriceList filePath = do
    content <- readFile filePath
    return $ parMap rpar read $ lines content

readFilePricesHistory :: FilePath -> [String] -> IO StockPricesHistory
readFilePricesHistory basePath tickers = do
    histories <- mapM (\t -> readFilePriceList $ basePath ++ "/" ++ t ++ ".txt") tickers
    let lens = map length histories
    if all (== head lens) lens
        then return $ newStockPricesHistory histories
        else error $ "readFilePricesHistory: mismatched price history lengths: " ++ show (zip tickers lens)
