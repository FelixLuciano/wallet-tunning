module RandomWallet
    ( randomWalletIO
    -- , normalizeWallet
    , randomWallet
    ) where

import Wallet
    ( Wallet(..)
    , newWallet
    , validateWallet
    )

import System.Random
import Control.Monad 
import Numeric.LinearAlgebra

-- Create a random n-sized vector and turns into a Wallet
-- The function takes a random generator and a size n
-- and returns a random Wallet
randomWalletIO :: StdGen -> Int -> IO Wallet
randomWalletIO gen n = do
    let randomShares = take n $ randomRs (0.0, 1.0) gen
    let wallet = newWallet randomShares
    let walletNormalized = wallet / scalar (sumElements wallet)
    if validateWallet walletNormalized
        then return wallet
        else randomWalletIO gen n

randomWallet :: Int -> IO Wallet
randomWallet n = do
    gen <- newStdGen
    randomWalletIO gen n
