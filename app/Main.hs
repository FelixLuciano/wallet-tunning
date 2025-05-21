module Main (main) where

import Wallet
    ( newStockPricesHistory
    , validateWallet
    , walletSharpeRatio
    )

import RandomWallet
    ( randomWallet
    )

import Numeric.LinearAlgebra

main :: IO ()
main = do
    myWallet <- RandomWallet.randomWallet 4
    putStrLn $ "My wallet: " ++ show myWallet

    putStrLn $ "Walet assets Shares: " ++ show (sumElements myWallet)

    let isValid = validateWallet myWallet
    putStrLn $ "Is the wallet valid? " ++ show isValid

    let stockPrices = Wallet.newStockPricesHistory [ [100, 200, 300, 400],
                                                     [105, 210, 310, 410],
                                                     [110, 220, 320, 420]
                                                   ]

    let volatility = walletSharpeRatio myWallet stockPrices
    putStrLn $ "Sharpe Ratio: " ++ show volatility
