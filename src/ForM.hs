module ForM (listForM_, vecForM_, streamM_, recM_) where

import Data.Foldable (forM_)
import Data.Vector.Fusion.Stream.Monadic qualified as MS
import Data.Vector.Unboxed qualified as U

listForM_ :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
listForM_ l r f = do
  forM_ [l .. r] $ \i -> do
    f i

vecForM_ :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
vecForM_ l r f = do
  U.forM_ (U.enumFromN l (r + 1 - l)) $ \i -> do
    f i

streamM_ :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
streamM_ l r = flip MS.mapM_ (rangeMS l r)

{-# INLINE [1] rangeMS #-}
rangeMS :: (Monad m) => Int -> Int -> MS.Stream m Int
rangeMS !l !r = MS.Stream step l
  where
    {-# INLINE [0] step #-}
    step x
      | x <= r = return $ MS.Yield x (x + 1)
      | otherwise = return MS.Done

recM_ :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
recM_ l r f = inner l
  where
    inner i
      | i > r = return ()
      | otherwise = do
          f i
          inner (i + 1)
