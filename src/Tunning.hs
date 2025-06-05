module Tunning
    ( tuneWallet
    , tuneTickers
    ) where

import Wallet
    ( Wallet
    , walletSharpeRatio
    )

import Tickers
    ( Tickers
    , StockPricesHistory
    )

import Data.List
    ( foldl1'
    , maximumBy
    )

import Data.Ord
    ( comparing
    )

import Control.Parallel.Strategies
    ( parMap
    , rpar
    , using
    , rdeepseq
    )

import Numeric.LinearAlgebra
    ( cols
    )

import RandomWallet
    ( randomWallets
    )

import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (deepseq)

type R_ = (Double, Wallet, Tickers)

tuneSharpeRatio :: [Wallet] -> StockPricesHistory -> (Double, Wallet)
tuneSharpeRatio wallets stockPricesHistory =
    max' $ parMap rpar (\w -> (walletSharpeRatio w stockPricesHistory, w)) wallets
    where
        max' = foldl1' (\acc x -> if let (a,_) = acc; (b,_) = x in b > a then x else acc)

tuneWallet :: Int -> Int -> Tickers -> StockPricesHistory -> R_
tuneWallet n seed tickers prices =
    let wallets = randomWallets nStocks n seed
        (sharpeRatio, wallet) =  tuneSharpeRatio wallets prices
    in (sharpeRatio, wallet, tickers)
    where
        nStocks = cols prices

tuneTickers :: Int -> Int -> [(Tickers, StockPricesHistory)] -> (IO R_ -> IO R_) -> IO R_
tuneTickers n seed selections sem = do
    let total = length selections
        enumerated = zip [1..] selections

    results <- mapConcurrently (\(i, (t, p)) ->
        sem $ do
            let result = tuneWallet n seed t p
            result `deepseq` when (i `mod` 100 == 0) (putStrLn $ show i ++ "/" ++ show total)
            return result
      ) enumerated

    return $ max' results
  where
    max' = foldl1' (\acc x -> if let (a,_,_) = acc; (b,_,_) = x in b > a then x else acc)
    when cond action = if cond then action else return ()
