module Utils where

-- non chess related utilities

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . map (take n) . iterate (drop n)

surround :: String -> String -> String
surround s str = s ++ str ++ s