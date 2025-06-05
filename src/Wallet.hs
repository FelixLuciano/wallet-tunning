module Wallet
    ( AssetShare
    , Wallet
    , ReturnRate
    , ReturnRateHistory
    , newWallet
    , validateWallet
    , stockReturn
    , stockReturns
    , walletDalyReturns
    , walletYearlyMeanReturn
    , walletVolatility
    , walletYearlyVolatility
    , walletSharpeRatio
    ) where

import Tickers
    ( StockPrice
    , StockPriceHistory
    , StockPricesHistory
    )

import Numeric.LinearAlgebra
    ( Matrix
    , Vector
    , fromColumns
    , fromList
    , fromLists
    , scalar
    , size
    , subVector
    , sumElements
    , toColumns
    , tr
    , toList
    , (<>)
    , (#>)
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    )

type AssetShare = Double
type Wallet = Vector AssetShare

type ReturnRate = Double
type ReturnRateHistory = Vector ReturnRate

newWallet :: [Double] -> Wallet
newWallet w = fromList w

validateWallet :: Wallet -> Bool
validateWallet wallet = allSharesValid && totalSharesValid
    where
        allSharesValid = size wallet < 5 || all (<= 0.2) (toList wallet)
        totalSharesValid = abs (sumElements wallet - 1.0) < 1e-9

stockReturn :: StockPriceHistory -> ReturnRateHistory
stockReturn prices = (subVector 1 n prices) / (subVector 0 n prices) - 1.0
    where
        n = size prices - 1

stockReturns :: StockPricesHistory -> Matrix ReturnRate
stockReturns prices = fromColumns $ parMap rpar stockReturn $ toColumns prices

walletDalyReturns :: Wallet -> StockPricesHistory -> Vector ReturnRate
walletDalyReturns wallet prices = stockReturns prices #> wallet

walletYearlyMeanReturn :: Wallet -> StockPricesHistory -> ReturnRate
walletYearlyMeanReturn wallet prices = (sqrt 252) * mean returns
    where
        returns = walletDalyReturns wallet prices

stockPriceVolatility :: StockPriceHistory -> Vector Double
stockPriceVolatility prices = prices - (scalar $ mean prices)

stockPricesVolatility :: StockPricesHistory -> Matrix Double
stockPricesVolatility prices = fromColumns $ parMap rpar stockPriceVolatility $ toColumns prices

walletVolatility :: Wallet -> StockPricesHistory -> Double
walletVolatility wallet prices = sqrt . sumElements $ wallet * sigma #> wallet
    where
        v = stockPricesVolatility prices
        n = fromIntegral (size wallet - 1)
        sigma = tr v Numeric.LinearAlgebra.<> v / n

walletYearlyVolatility :: Wallet -> StockPricesHistory -> Double
walletYearlyVolatility wallet prices = (sqrt 252) * walletVolatility wallet prices

walletSharpeRatio :: Wallet -> StockPricesHistory -> Double
walletSharpeRatio wallet prices = return / volatility
    where
        return = walletYearlyMeanReturn wallet prices
        volatility = walletYearlyVolatility wallet prices

-- UTILS

prefixVector x v = fromList (x : toList v)

mean :: Vector Double -> Double
mean x = sumElements x / n
    where
        n = fromIntegral (size x - 1)
