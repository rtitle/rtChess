module Utils where

-- non chess related utilities

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

surround :: String -> String -> String
surround s str = s ++ str ++ s

(&&&) :: Bool -> Bool -> Bool -> Bool
(&&&) a b c = a && b && c

(&&&&) :: Bool -> Bool -> Bool -> Bool -> Bool
(&&&&) a b c d = a && b && c && d