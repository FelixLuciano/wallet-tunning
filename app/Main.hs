module Main (main) where

import Tickers
    ( getStocksSelection
    )

import TickersIO
    ( readFileTickers
    , readFilePricesHistory
    )

import TunningIO
    ( tuneTickersIO
    )

main :: IO ()
main = do
    tickers <- TickersIO.readFileTickers "./data/index/US30-tickers.txt"
    prices <- TickersIO.readFilePricesHistory "./data/prices" tickers

    let selections = getStocksSelection 2 tickers prices
    putStrLn $ "Amount of possible selections: " ++ show (length selections)

    (myWallet, sharpeRatio) <- TunningIO.tuneTickersIO 100 selections
    putStrLn $ "Sharpe Ratio: " ++ show sharpeRatio
    putStrLn $ "Wallet: " ++ show myWallet
