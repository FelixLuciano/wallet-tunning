module Main (main) where

import Wallet
    ( newStockPricesHistory
    , validateWallet
    , walletSharpeRatio
    )

import Tickers
    ( getTickersSelection
    )

import TickersIO
    ( fromFileIO
    )

import WalletIO
    ( readFilePricesHistory
    )

import WalletTune
    ( tryMaxWalletSharpeRatio
    )

main :: IO ()
main = do
    myTickers <- TickersIO.fromFileIO "./data/index/US30-tickers.txt"

    let selections = getTickersSelection 25 myTickers
    putStrLn $ "Amount of possible selections: " ++ show (length selections)

    prices <- readFilePricesHistory "./data/prices" (selections!!0)

    (myWallet, sharpeRation) <- tryMaxWalletSharpeRatio 1000 prices
    putStrLn $ "Sharpe Ratio: " ++ show sharpeRation
    putStrLn $ "Wallet: " ++ show myWallet
