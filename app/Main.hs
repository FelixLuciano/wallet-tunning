module Main (main) where

import Tickers
    ( getStocksSelection
    )

import TickersIO
    ( readFileTickers
    , readFilePricesHistory
    )

import Tunning
    ( tuneTickers
    )

import Data.Time.Clock.POSIX
    ( getPOSIXTime
    )


import Control.Concurrent
    ( getNumCapabilities
    )

import Control.Concurrent.QSem
    ( newQSem
    , waitQSem
    , signalQSem
    )

import Control.Exception
    ( bracket_
    )

import Numeric
    ( showFFloat
    )


import Numeric.LinearAlgebra
    ( toList
    )

mainSeed :: Int
mainSeed = 24

main :: IO ()
main = do
    tickers <- TickersIO.readFileTickers "./data/index/US30-tickers.txt"
    prices <- TickersIO.readFilePricesHistory "./data/prices" tickers

    let selections = getStocksSelection 25 tickers prices
    let size = length selections
    putStrLn $ "Amount of possible selections: " ++ show size

    numCaps <- getNumCapabilities
    sem <- newQSem numCaps
    let withSemaphore = bracket_ (waitQSem sem) (signalQSem sem)

    startTime <- getPOSIXTime
    (sharpeRatio, myWallet, myTickers) <- Tunning.tuneTickers 1000 mainSeed selections withSemaphore
    endTime <- getPOSIXTime

    let elapsed = round (endTime - startTime) :: Integer
    let weights = toList myWallet

    putStrLn $ "Elapsed time: " ++ show elapsed ++ " s"
    putStrLn $ "Sharpe Ratio: " ++ Numeric.showFFloat (Just 5) sharpeRatio ""

    putStrLn "Weights:"
    mapM_ (\(t, w) -> putStrLn $ "- " ++ t ++ ": " ++ showD (w * 100) ++ "%") $ zip myTickers weights

    where
        showD d = Numeric.showFFloat (Just 2) d ""
