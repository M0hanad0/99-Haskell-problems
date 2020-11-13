module ListsContinued where

import           Lists (runLengthEncode)

data RLEData a = Single a | Multiple Integer a deriving (Show, Eq)

runLenEncodeModified :: Eq a => [a] -> [RLEData a]
runLenEncodeModified = map encodeHelper . runLengthEncode
 where
  encodeHelper (1, x) = Single x
  encodeHelper (n, x) = Multiple n x

runLenDecodeModified :: Eq a => [RLEData a] -> [a]
runLenDecodeModified = concatMap auxDecode
 where
  auxDecode (Single x    ) = [x]
  auxDecode (Multiple n x) = replicate (fromIntegral n) x

duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

replicateElement :: Integral b => b -> a -> [a]
replicateElement 0 _ = []
replicateElement 1 x = [x]
replicateElement n x = x : replicateElement (n - 1) x

anotherReplicate :: Integral b => b -> a -> [a]
anotherReplicate n = take (fromIntegral n) . repeat

replicateList :: Integral b => b -> [a] -> [a]
replicateList n = concatMap (replicate $ fromIntegral n)

dropEvery :: Integral b => [a] -> b -> [a]
dropEvery [] _ = []
dropEvery (x : xs) n | n == 1    = xs
                     | otherwise = x : dropEvery xs (n - 1)

split :: Integral b => [a] -> b -> ([a], [a])
split xs n = (take (fromIntegral n) xs, drop (fromIntegral n) xs)

slice :: Integral b => [a] -> b -> b -> [a]
slice xs i k | i > 0     = final
             | otherwise = []
 where
  firstSplit = drop (fromIntegral (i - 1)) xs
  final      = take (fromIntegral (k - i + 1)) firstSplit

rotate :: Integral b => [a] -> b -> [a]
rotate xs 0 = xs
rotate xs n | n > 0     = drop (fromIntegral n) xs ++ take (fromIntegral n) xs
            | otherwise = drop len xs ++ take len xs
  where len = fromIntegral (length xs + fromIntegral n)

removeAt :: Integral b => b -> [a] -> (Maybe a, [a])
removeAt n xs
  | n > 0 && n <= fromIntegral (length xs)
  = (Just (xs !! index), take index xs ++ drop (fromIntegral n) xs)
  | otherwise
  = (Nothing, xs)
  where index = fromIntegral $ n - 1
