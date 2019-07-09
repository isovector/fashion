module Halton where

import Control.Monad
import Control.Monad.ST
import Data.Function (fix)
import Data.STRef
import Diagrams.Prelude hiding (ix)


oneHalton :: Int -> Int -> Double
oneHalton b ix = runST $ do
  f <- newSTRef @Double 1
  r <- newSTRef @Double 0
  i <- newSTRef ix

  fix $ \me -> do
    ival <- readSTRef i
    when (ival > 0) $ do
      modifySTRef f (/ fromIntegral b)
      fval <- readSTRef f
      ival' <- readSTRef i
      modifySTRef r $ (+ (fval * fromIntegral (ival' `mod` b)))
      modifySTRef i $ fromIntegral . floor . (/ fromIntegral b) . fromIntegral
      me

  readSTRef r
{-# INLINE oneHalton #-}


halton :: Int -> Int -> [V2 Double]
halton x y = drop 1 $ do
  i <- [0..]
  pure $ (oneHalton x i - 0.5) ^& (oneHalton y i - 0.5)

