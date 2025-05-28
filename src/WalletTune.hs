module WalletTune
    ( maxWalletSharpeRatio
    , tryMaxWalletSharpeRatio
    ) where

import Wallet
    ( Wallet
    , StockPricesHistory
    , newStockPricesHistory
    , validateWallet
    , walletSharpeRatio
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


maxWalletSharpeRatio :: [Wallet] -> StockPricesHistory -> (Wallet, Double)
maxWalletSharpeRatio wallets stockPricesHistory =
    argmax $ parMap rpar (\w -> (w, walletSharpeRatio w stockPricesHistory)) wallets
        where
            argmax xs = foldl1 (\(w1, s1) (w2, s2) -> if s1 > s2 then (w1, s1) else (w2, s2)) xs

tryMaxWalletSharpeRatio :: Int -> StockPricesHistory -> IO (Wallet, Double)
tryMaxWalletSharpeRatio n stockPricesHistory = do
    wallets <- randomWallets nStocks n
    return $ maxWalletSharpeRatio wallets stockPricesHistory
        where 
            nStocks = cols stockPricesHistory
