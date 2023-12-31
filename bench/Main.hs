import Criterion.Main
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.IORef
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import ForM
import Knapsack
-- import Test.Tasty.Bench
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
  ((!_nItems, !w), !input) <- readInput
  let nIter = 10 ^ (8 :: Int)
  !ref <- newIORef (0 :: Int)

  -- Maybe prefer non-point-free version for more acculate result
  defaultMain
    [ bgroup
        "knapsack"
        [ bench "dense-unboxed-vector" $ whnf (denseU w) input,
          bench "dense-boxed-vector" $ whnf (denseV w) input,
          bench "sparse-list" $ whnf (sparseList w) input,
          bench "sparse-list-forced" $ whnf (sparseListForced w) input,
          bench "sparse-int-map" $ whnf (sparseIM w) input,
          bench "sparse-int-map-forced" $ whnf (sparseIMForced w) input,
          bench "sparse-unboxed-vector" $ whnf (sparseU w) input,
          bench "sparse-mono-list" $ whnf (sparseMonoList w) input,
          bench "sparse-mono-unboxed-vector" $ whnf (sparseMonoU w) input
        ],
      bgroup
        "times(N=10^8)"
        [ bench "list-iterate" $ whnf (\n -> iterList n (+ 1) (0 :: Int)) nIter,
          bench "unboxed-vector-iterate" $ whnf (\n -> iterU n (+ 1) (0 :: Int)) nIter,
          -- FIXME: probably wrong
          bench "bimap-tuple" $ whnf (\n -> bimapTuple n (+ 1) (0 :: Int)) nIter,
          bench "rec" $ whnf (\n -> iterRec n (+ 1) (0 :: Int)) nIter
        ],
      bgroup
        "forM_(N=10^8)"
        [ bench "list-forM_" $ nfIO $ listForM_ 0 (nIter - 1) (writeIORef ref),
          bench "vec-forM_" $ nfIO $ vecForM_ 0 (nIter - 1) (writeIORef ref),
          bench "streamM_" $ nfIO $ streamM_ 0 (nIter - 1) (writeIORef ref),
          bench "recM_" $ nfIO $ recM_ 0 (nIter - 1) (writeIORef ref)
        ]
    ]
