module Main (main) where

import Wallet
    ( Wallet(..)
    , newWallet
    , newStockPricesHistory
    , validateWallet
    , walletDalyReturns
    , walletYearlyMeanReturn
    , walletVolatility
    )

main :: IO ()
main = do
    let myWallet = Wallet.newWallet [0.25, 0.25, 0.25, 0.25]
    putStrLn $ "My wallet: " ++ show myWallet

    let isValid = validateWallet myWallet
    putStrLn $ "Is the wallet valid? " ++ show isValid

    let stockPrices = Wallet.newStockPricesHistory [ [100, 200, 300, 400],
                                                     [105, 210, 310, 410],
                                                     [110, 220, 320, 420]
                                                   ]

    let returns = walletDalyReturns myWallet stockPrices
    putStrLn $ "Wallet daily returns: " ++ show returns

    let yearlyReturns = walletYearlyMeanReturn myWallet stockPrices
    putStrLn $ "Wallet mean yearly return: " ++ show yearlyReturns

    let volatility = walletVolatility myWallet stockPrices
    putStrLn $ "Wallet volatility: " ++ show volatility
