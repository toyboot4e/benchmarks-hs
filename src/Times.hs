module Times (iterList, iterU, iterRec, bimapTuple) where

import Data.Bifunctor
import Data.Vector.Unboxed qualified as U

iterList :: Int -> (a -> a) -> a -> a
iterList n f s0 = (!! n) $ iterate f s0

iterU :: U.Unbox a => Int -> (a -> a) -> a -> a
iterU n f s0 = U.last $ U.iterateN n f s0

iterRec :: Int -> (a -> a) -> a -> a
iterRec n f s0 = inner 0 s0
  where
    inner i !s | i == n = s
    inner x !s = inner (x + 1) $! f s

bimapTuple :: Int -> (a -> a) -> a -> a
bimapTuple !n !f !s0 = snd $! until ((== n) . fst) (bimap succ f) (0 :: Int, s0)
