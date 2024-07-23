import Data.List
import Data.Char

-- see type  => :t
-- load file => :l class1.hs
-- auto type => :set +t

-- count unique characters in a string
-- countUniqueChars = length . group . sort

add :: Num a => a -> a -> a
add x y = x + y

manutdLegend 7 = "Eric Cantona"
manutdLegend 11 = "Eric Cantona"
manutdLegend 6 = "Eric Cantona"
manutdLegend _ = "Eric Cantona"

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- not common
fac n
    | n == 0 = 1
    | n > 0 = n * fac (n - 1)

grade :: Int -> Char
grade score
    | score >= 90 = 'A'
    | score >= 80 = 'B'
    | score >= 70 = 'C'
    | otherwise = 'F'

count :: [a] -> Int
count [] = 0
count (_:xs) = 1 + count xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- count unique word
input = "this cat this bat this rat"
-- count = length
-- unique = nub
         -- The name "num" means essense
countUniqueWords :: String -> Int
countUniqueWords = count . unique . words
    where
        unique = nub

-- if
-- if (8>5) then 10 else 20
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' pred (x:xs) = if (pred x) then x:filter' pred xs else filter' pred xs