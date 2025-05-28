module WalletIO
    ( readFilePriceList
    , readFilePricesHistory
    , randomWallet
    , randomWallets
    ) where

import Wallet
    ( Wallet
    , StockPrice
    , StockPricesHistory
    , newWallet
    , validateWallet
    , newStockPricesHistory
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    )

import Control.Monad
    ( replicateM
    )

import System.Random
    (randomRIO
    )

import System.Random.Shuffle
    (shuffleM
    )

import Numeric.LinearAlgebra
    ( scalar
    , sumElements
    )


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

-- Built with the assistance of ChatGPT (GPT-4.1). Prompt:
-- Generates an n-size list of floats where each value is randomly chosen between 0 and 0.2
-- but does not exceed the remaining sum (starting from 1). The sum of the list is equal 1.
-- At the end, the list is shuffled randomly to avoid any specific order.
randomDoubleListN :: Int -> IO [Double]
randomDoubleListN n = do
    ll <- aux n 1 []
    shuffleM ll
    where
        aux 0 _ xs = return xs
        aux n rem xs = do
            let maxVal = min 0.2 rem
            x <- if n == 1
                then return rem
                else randomRIO (0.001, maxVal)
            xs' <- aux (n - 1) (rem - x) (x : xs)
            return xs'

randomWallet :: Int -> IO Wallet
randomWallet n = do
    randomShares <- randomDoubleListN n
    let wallet = newWallet randomShares
    let walletNormalized = wallet / scalar (sumElements wallet)
    if validateWallet walletNormalized
        then return wallet
        else randomWallet n

randomWallets :: Int -> Int -> IO [Wallet]
randomWallets n l = do
    replicateM l (randomWallet n)