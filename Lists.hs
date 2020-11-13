-- The MIT License (MIT)
--
-- Copyright (c) 2020 Mohanad Hosam
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
--                                                                                                                                                                                        the following conditions:
--
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
