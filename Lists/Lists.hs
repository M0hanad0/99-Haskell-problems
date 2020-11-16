module Lists where
import           Data.Maybe (fromMaybe)

data NestedList a = Elem a | List [NestedList a]

myLast :: [a] -> Maybe a
myLast []       = Nothing
myLast [x     ] = Just x
myLast (x : xs) = Just $ fromMaybe x (myLast xs)

foldrLast :: [a] -> Maybe a
foldrLast [] = Nothing
foldrLast xs = Just $ foldr1 (\_ y -> y) xs

butLast :: [a] -> Maybe a
butLast []       = Nothing
butLast [x, _]   = Just x
butLast (_ : xs) = butLast xs

elementAt :: (Integral i) => i -> [a] -> Maybe a
elementAt _ [] = Nothing
elementAt i (x : xs) | i == 1    = Just x
                     | otherwise = elementAt (i - 1) xs

myLength :: [a] -> Integer
myLength xs = go xs 0
 where
  go []       acc = acc
  go (_ : ys) acc = go ys (acc + 1)
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

flatten :: NestedList a -> [a]
flatten (Elem a       ) = [a]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)
flatten (List []      ) = []

compress :: Eq a => [a] -> [a]
compress []               = []
compress [x             ] = [x]
compress (x : ys@(y : _)) = if x == y then compress ys else x : compress ys

pack :: Eq a => [a] -> [[a]]
pack []       = []
pack (x : xs) = let (first, rest) = span (== x) xs in (x : first) : pack rest

runLengthEncode :: (Eq a, Integral b) => [a] -> [(b, a)]
runLengthEncode = map (\x -> (fromIntegral $ length x, head x)) . pack


runLengthDecode :: [(Int, b)] -> [b]
runLengthDecode = concatMap (uncurry replicate)
