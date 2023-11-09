import Criterion.Main
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.IORef
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import ForM
import Knapsack
import Times

-- Getting started
-- <http://www.serpentine.com/criterion/tutorial.html>

ints2 :: BS.ByteString -> ((Int, Int), BS.ByteString)
ints2 !bs0 =
  let (!a1, !bs1) = fromJust $ BS.readInt (BS.dropWhile isSpace bs0)
      (!a2, !bs2) = fromJust $ BS.readInt (BS.dropWhile isSpace bs1)
   in ((a1, a2), bs2)

readInput :: IO ((Int, Int), U.Vector (Int, Int))
readInput = do
  -- TODO: read into line instead
  !bs <- BS.readFile "bench/1_05"
  let ((!n, !maxW), !bs') = ints2 bs
  let vws = U.unfoldrExactN n ints2 bs'
  return ((n, maxW), vws)

main :: IO ()
main = do
  ((!n, !w), !input) <- readInput
  let nIter = 10_000_000
  !ref <- newIORef (0 :: Int)

  defaultMain
    [ bgroup
        "knapsack"
        [ bench "dense-unboxed-vector" $ (`whnf` input) (\vws -> denseU w vws),
          bench "dense-boxed-vector" $ (`whnf` input) (\vws -> denseV w vws),
          bench "sparse-list" $ (`whnf` input) (\vws -> sparseList w vws),
          bench "sparse-list-forced" $ (`whnf` input) (\vws -> sparseListForced w vws),
          bench "sparse-int-map" $ (`whnf` input) (\vws -> sparseIM w vws),
          bench "sparse-int-map-forced" $ (`whnf` input) (\vws -> sparseIMForced w vws),
          bench "sparse-unboxed-vector" $ (`whnf` input) (\vws -> sparseU w vws),
          bench "sparse-mono-list" $ (`whnf` input) (\vws -> sparseMonoList w vws),
          bench "sparse-mono-unboxed-vector" $ (`whnf` input) (\vws -> sparseMonoU w vws)
        ],
      bgroup
        "times"
        [ bench "list-iterate" $ whnf (iterList nIter (+ 1)) (0 :: Int),
          bench "unboxed-vector-iterate" $ whnf (iterU nIter (+ 1)) (0 :: Int),
          bench "bimap-tuple" $ whnf (bimapTuple n (+ 1)) (0 :: Int),
          bench "rec" $ whnf (iterRec nIter (+ 1)) (0 :: Int)
        ],
      bgroup
        "forM_"
        [ bench "list-forM_" $ nfIO $ listForM_ 0 (nIter - 1) (writeIORef ref),
          bench "vec-forM_" $ nfIO $ vecForM_ 0 (nIter - 1) (writeIORef ref),
          bench "streamM_" $ nfIO $ streamM_ 0 (nIter - 1) (writeIORef ref),
          bench "recM_" $ nfIO $ recM_ 0 (nIter - 1) (writeIORef ref)
        ]
    ]
