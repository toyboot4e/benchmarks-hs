module Knapsack (denseU, denseV, sparseList, sparseU, sparseListMono) where

import Data.Bool (bool)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U

-- | https://atcoder.jp/contests/dp/tasks/dp_d

denseU :: Int -> U.Vector (Int, Int) -> Int
denseU maxW = U.maximum . U.foldl' step s0
  where
    s0 = U.generate (maxW + 1) $ bool minBound 0 . (== 0)
    step sofar (!dw, !dv) = U.imap f sofar
      where
        f :: Int -> Int -> Int
        f w0 v0 = max v0 $ maybe 0 (dv +) (sofar U.!? (w0 - dw))

denseV :: Int -> U.Vector (Int, Int) -> Int
denseV maxW = V.maximum . U.foldl' step s0
  where
    s0 = V.generate (maxW + 1) $ bool minBound 0 . (== 0)
    step sofar (!dw, !dv) = V.imap f sofar
      where
        f :: Int -> Int -> Int
        f w0 v0 = max v0 $ maybe 0 (dv +) (sofar V.!? (w0 - dw))

sparseList :: Int -> U.Vector (Int, Int) -> Int
sparseList maxW = maximum . map fst . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step wvs (!dw, !dv) =
      merge wvs $ filter ((<= maxW) . fst) $ map (\(!w, !v) -> (w + dw, v + dv)) wvs

    merge :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge xs [] = xs
    merge [] xs = xs
    merge xxs@(!x : xs) yys@(!y : ys) = case compare (fst x) (fst y) of
      LT -> x : merge xs yys
      GT -> y : merge xxs ys
      EQ -> (fst x, max (snd x) (snd y)) : merge xs ys

sparseU :: Int -> U.Vector (Int, Int) -> Int
sparseU maxW = U.maximum . U.map fst . U.foldl' step s0
  where
    s0 = U.singleton (0 :: Int, 0 :: Int)
    step wvs (!dw, !dv) =
      U.unfoldr merge . (,) wvs $ U.filter ((<= maxW) . fst) $ U.map (\(!w, !v) -> (w + dv, v + dw)) wvs

    merge :: (U.Vector (Int, Int), U.Vector (Int, Int)) -> Maybe ((Int, Int), (U.Vector (Int, Int), U.Vector (Int, Int)))
    merge (!xs, !ys) = case (U.uncons xs, U.uncons ys) of
      (Nothing, Nothing) -> Nothing
      (Just (!x, !xs'), Nothing) -> Just (x, (xs', U.empty))
      (Nothing, Just (!y, !ys')) -> Just (y, (U.empty, ys'))
      (Just (!x, !xs'), Just (!y, !ys')) -> case compare (fst x) (fst y) of
        LT -> Just (x, (xs', ys))
        GT -> Just (y, (xs, ys'))
        EQ -> Just ((fst x, max (snd x) (snd y)), (xs', ys'))

sparseListMono :: Int -> U.Vector (Int, Int) -> Int
sparseListMono maxW = maximum . map fst . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step wvs (!dw, !dv) =
      merge 0 wvs $ filter ((<= maxW) . fst) $ map (\(!v, !w) -> (v + dv, w + dw)) wvs

    merge :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge _ xs [] = xs
    merge _ [] ys = ys
    merge maxV xs@(x : xrest) ys@(y : yrest)
      | snd x < maxV = merge maxV xrest ys
      | snd y < maxV = merge maxV xs yrest
      | otherwise = case compare (fst x) (fst y) of
          LT -> x : merge (max maxV (snd x)) xrest ys
          GT -> y : merge (max maxV (snd y)) xs yrest
          EQ ->
            let !v = max (snd x) (snd y)
                !maxV' = max v maxV
             in (fst x, v) : merge maxV' xrest yrest
