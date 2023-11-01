module Lib (knapsackU, knapsackV, knapsackList, knapsackListMono) where

import Data.Bool (bool)
import Data.Vector.Unboxed qualified as U
import Data.Vector qualified as V

knapsackU :: Int -> U.Vector (Int, Int) -> Int
knapsackU maxW = U.maximum . U.foldl' step s0
  where
    s0 = U.generate (maxW + 1) $ bool minBound 0 . (== 0)
    step sofar (!dw, !dv) = U.imap f sofar
      where
        f :: Int -> Int -> Int
        f w0 v0 =
          let !v = maybe 0 (dv +) (sofar U.!? (w0 - dw))
           in max v v0

knapsackV :: Int -> U.Vector (Int, Int) -> Int
knapsackV maxW = V.maximum . U.foldl' step s0
  where
    s0 = V.generate (maxW + 1) $ bool minBound 0 . (== 0)
    step sofar (!dw, !dv) = V.imap f sofar
      where
        f :: Int -> Int -> Int
        f w0 v0 =
          let !v = maybe 0 (dv +) (sofar V.!? (w0 - dw))
           in max v v0

knapsackList :: Int -> U.Vector (Int, Int) -> Int
knapsackList maxW = maximum . map fst . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step vws (!dw, !dv) =
      merge vws $ filter ((<= maxW) . snd) $ map (\(!v, !w) -> (v + dv, w + dw)) vws

    merge [] xs = xs
    merge xs [] = xs
    merge xxs@(!x : xs) yys@(!y : ys) = case compare (snd x) (snd y) of
      LT -> x : merge xs yys
      GT -> y : merge xxs ys
      EQ -> (max (fst x) (fst y), snd x) : merge xs ys

knapsackListMono :: Int -> U.Vector (Int, Int) -> Int
knapsackListMono maxW = maximum . map fst . U.foldl' step s0
  where
    s0 = [(0, 0)] :: [(Int, Int)]
    step wvs (!dw, !dv) =
      merge 0 wvs $ filter ((<= maxW) . fst) $ map (\(!v, !w) -> (v + dv, w + dw)) wvs

    merge :: Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    merge _ [] ys = ys
    merge _ xs [] = xs
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
