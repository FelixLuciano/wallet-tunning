module RandomWallet
    ( randomWallet
    , randomWallets
    ) where

import Wallet
    ( Wallet
    , newWallet
    , validateWallet
    )

import Tickers
    ( Tickers
    , StockPriceHistory
    , newStockPricesHistory
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    )

import System.Random
    ( mkStdGen
    , randomR
    , randoms
    )

import System.Random.Shuffle
    (shuffle'
    )

import Numeric.LinearAlgebra
    ( scalar
    , sumElements
    )

-- Built with the assistance of ChatGPT (GPT-4.1). Prompt:
-- Generates an n-size list of floats where each value is randomly chosen between 0 and 0.2
-- but does not exceed the remaining sum (starting from 1). The sum of the list is equal 1.
-- At the end, the list is shuffled randomly to avoid any specific order.
randomDoubleListN :: Int -> Int -> [Double]
randomDoubleListN n seed
    | n <= 0 = []
    | otherwise =
        let gen = mkStdGen seed
            shuffled = shuffle' (aux n 1 [] gen) n gen
            norm = sum shuffled
        in map (/ norm) shuffled
        where
            aux 1 x xs _ = x : xs
            aux k y xs gen =
                let maxVal = min 0.2 y
                    (x, gen') = randomR (0.0, maxVal) gen
                in aux (k - 1) (y - x) (0.001 + x : xs) gen'

randomWallet :: Int -> Int -> Wallet
randomWallet n seed =
    let wallet = newWallet $ randomDoubleListN n seed
    in if validateWallet wallet
        then wallet
        else randomWallet n seed

randomWallets :: Int -> Int -> Int -> [Wallet]
randomWallets n l seed =
    let seeds = take l $ randoms (mkStdGen seed)
    in parMap rpar (randomWallet n) seeds
