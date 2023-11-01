import Criterion.Main
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Lib qualified as L

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
  defaultMain
    [ bgroup
        "knapsack"
        [ bench "unboxed-vector" $ whnf (L.knapsackU w) input,
          bench "boxed-vector" $ whnf (L.knapsackV w) input,
          bench "list" $ whnf (L.knapsackList w) input,
          bench "list-mono" $ whnf (L.knapsackListMono w) input
          -- , bench "9" $ whnf fib 9
          -- , bench "11" $ whnf fib 11
        ]
    ]
