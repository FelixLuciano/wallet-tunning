module Wallet
    ( AssetShare
    , Wallet
    , StockPrice
    , ReturnRate
    , newWallet
    , newStockPricesHistory
    , validateWallet
    , sharpeRatio
    , stockReturn
    , stockReturns
    , walletDalyReturns
    , walletYearlyMeanReturn
    , walletVolatility
    , walletYearVolatility
    ) where

import Numeric.LinearAlgebra

type AssetShare = Float
type Wallet = Vector AssetShare

type StockPrice = Float
type StockPriceHistory = Vector StockPrice
type StockPricesHistory = Matrix StockPrice

type ReturnRate = Float
type ReturnRateHistory = Vector ReturnRate

newWallet :: [AssetShare] -> Wallet
newWallet w = fromList w

newStockPricesHistory :: [[StockPrice]] -> StockPricesHistory
newStockPricesHistory p = fromLists p

validateWallet :: Wallet -> Bool
validateWallet wallet = allSharesValid && totalSharesValid
  where
    -- Check if all asset shares are less than or equal to 20%
    -- unless you have less than 5 assets, which makes impossible
    allSharesValid = size wallet < 5 || all (<= 0.2) (toList wallet)
    -- Check if the sum of all asset shares is equal to 100%
    totalSharesValid = sumElements wallet == 1.0

sharpeRatio :: Float -> Float -> Float
sharpeRatio riskReturn riskFreeRate = (riskReturn - riskFreeRate) / riskReturn

stockReturn :: StockPriceHistory -> ReturnRateHistory
stockReturn prices = currentPrice / previousPrice - 1.0
  where
    n = size prices - 1
    currentPrice = prefixVector 1 $ subVector 1 n prices
    previousPrice = prefixVector 1 $ subVector 0 n prices

stockReturns :: Matrix StockPrice -> Matrix ReturnRate
stockReturns prices = fromColumns $ map stockReturn $ toColumns prices

walletDalyReturns :: Wallet -> Matrix StockPrice -> Vector ReturnRate
walletDalyReturns wallet prices = stockReturns prices #> wallet

walletYearlyMeanReturn :: Wallet -> Matrix StockPrice -> ReturnRate
walletYearlyMeanReturn wallet prices = (sqrt 252) * mean returns
  where
    returns = walletDalyReturns wallet prices

stockPriceVolatility :: Vector StockPrice -> Vector Float
stockPriceVolatility prices = prices - (scalar $ mean prices)

stockPricesVolatility :: Matrix StockPrice -> Matrix Float
stockPricesVolatility prices = fromColumns $ map stockPriceVolatility $ toColumns prices

walletVolatility :: Wallet -> Matrix StockPrice -> Float
walletVolatility wallet prices = sqrt . sumElements $ wallet * sigma #> wallet
  where
    v = stockPricesVolatility prices
    n = fromIntegral (size wallet - 1)
    sigma = tr v Numeric.LinearAlgebra.<> v / n

walletYearVolatility :: Wallet -> Matrix StockPrice -> Float
walletYearVolatility wallet prices = (sqrt 252) * walletVolatility wallet prices

-- UTILS

prefixVector x v = fromList (x : toList v)

mean :: Vector Float -> Float
mean x = sumElements x / n
  where
    n = fromIntegral (size x - 1)
