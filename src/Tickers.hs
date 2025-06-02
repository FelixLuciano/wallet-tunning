module Tickers
    ( Ticker
    , Tickers
    , StockPrice
    , StockPriceHistory
    , StockPricesHistory
    , newStockPricesHistory
    , getStocksSelection
    ) where

import Numeric.LinearAlgebra
    ( Vector
    , Matrix
    , toColumns
    , fromColumns
    , fromLists
    , tr
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    )

type Ticker = String
type Tickers = [Ticker]

type StockPrice = Double
type StockPriceHistory = Vector StockPrice
type StockPricesHistory = Matrix StockPrice

newStockPricesHistory :: [[StockPrice]] -> StockPricesHistory
newStockPricesHistory = tr . fromLists

-- subsequences of length n from list performance
-- From: https://stackoverflow.com/questions/21265454
-- At:   https://stackoverflow.com/a/21288092
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
    where
        subsequencesBySize [] = [[[]]]
        subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

getStocksSelection :: Int -> Tickers -> StockPricesHistory -> [(Tickers, StockPricesHistory)]
getStocksSelection n tickers prices =
    let s = subsequencesOfSize n (zip tickers (toColumns prices))
    in parMap rpar (\c -> (map fst c, fromColumns $ map snd c)) s
