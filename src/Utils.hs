{- |
Utils.hs
Non chess related utilities.
-}
module Utils where

-- |Splits a list into length-n pieces.
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- |Surrounds a String (second argument) with another String (first argument).
-- For example, surround "foo" "bar" = "foobarfoo"
surround :: String -> String -> String
surround s str = s ++ str ++ s

-- |3-paramater `and`.
(&&&) :: Bool -> Bool -> Bool -> Bool
(&&&) a b c = a && b && c

-- |4-parameter `and`.
(&&&&) :: Bool -> Bool -> Bool -> Bool -> Bool
(&&&&) a b c d = a && b && c && d