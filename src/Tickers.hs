module Tickers
    ( Ticker
    , Tickers
    , getTickersSelection
    ) where


type Ticker = String
type Tickers = [Ticker]

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

getTickersSelection :: Int -> Tickers -> [Tickers]
getTickersSelection n tickers = subsequencesOfSize n tickers
