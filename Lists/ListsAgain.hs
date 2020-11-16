module ListsAgain where

-- import           ListsContinued (slice)

insertAt :: Integral b => a -> [a] -> b -> [a]
insertAt x xs i = take idx xs ++ [x] ++ drop idx xs
  where
    idx = fromIntegral (i - 1)

range :: Integral a => a -> a -> [a]
range x y = take (fromIntegral (y - x + 1)) $ iterate (+ 1) x

-- anotherRange :: Integral a => a -> a -> [a]
-- anotherRange x y = slice (iterate (+ 1) x) 1 (y - x + 1)
