module TunningIO
    ( tuneWalletIO
    , tuneTickersIO
    ) where

import Wallet
    ( Wallet
    , walletSharpeRatio
    )

import Tickers
    ( Tickers
    , StockPricesHistory
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    )

import Numeric.LinearAlgebra
    ( cols
    )

import WalletIO
    ( randomWallets
    )


tuneSharpeRatio :: [Wallet] -> StockPricesHistory -> (Wallet, Double)
tuneSharpeRatio wallets stockPricesHistory =
    max' $ parMap rpar (\w -> (w, walletSharpeRatio w stockPricesHistory)) wallets
        where
            max' xs = foldl1 (\(w1, s1) (w2, s2) -> if s1 > s2 then (w1, s1) else (w2, s2)) xs

tuneWalletIO :: Int -> StockPricesHistory -> IO (Wallet, Double)
tuneWalletIO n stockPricesHistory = do
    wallets <- randomWallets nStocks n
    return $ tuneSharpeRatio wallets stockPricesHistory
        where 
            nStocks = cols stockPricesHistory

tuneTickersIO :: Int -> [(Tickers, StockPricesHistory)] -> IO (Wallet, Double)
tuneTickersIO n selections = do
    results <- mapM (\(t, p) -> tuneWalletIO n p) selections
    return $ max' $ results
        where
            max' xs = foldl1 (\(w1, s1) (w2, s2) -> if s1 > s2 then (w1, s1) else (w2, s2)) xs
