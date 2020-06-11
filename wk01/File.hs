import Data.Char(toLower)
import Data.List(group,sort,sortBy)

toCartesian :: (Double, Double) -> (Double, Double)
toCartesian = \(r, theta) -> let 
                               y = r * sin theta
                               x = r * cos theta
                             in (x, y) 
{- 
toCartesian (r, theta) = (x, y)
    where x = r * cos theta
          y = r * sin theta 
-}

twice :: (a -> a) -> (a -> a)
twice f a = f (f a) -- Equation 1

double :: Int -> Int
double = \x -> x * 2

{-
   twice twice double 3
== (twice twice double) 3  - Equation 1
== (twice (twice double)) 3 
== (twice quadruple) 3 - defn. of quadruple
== quadruple (quadruple 3) - Equation 1
== 48
-}

quadruple :: Int -> Int
quadruple = twice double -- defn of quadruple

breakIntoWords :: String -> [String]
breakIntoWords = words

convertIntoLowercase :: [[Char]] -> [String]
convertIntoLowercase = map (map toLower)

sortWords :: [String] -> [String]
sortWords = sort

type Run = (Int, String)
countAdjacentRuns :: [String] -> [Run]
countAdjacentRuns = convertToRuns . groupAdjacentRuns

groupAdjacentRuns :: [String] -> [[String]]
groupAdjacentRuns = group

convertToRuns :: [[String]] -> [Run]
convertToRuns = map (\ls -> (length ls, head ls))

sortByRunSize :: [Run] -> [Run]
sortByRunSize = sortBy (\(l1, w1) (l2, w2) -> compare l2 l1)

takeFirst :: Int -> [Run] -> [Run]
takeFirst = take

generateReport :: [Run] -> String
generateReport = unlines . map (\(l,w) -> w ++ ":" ++ show l)

mostCommonWords :: Int -> String -> String
mostCommonWords n =
     generateReport
   . takeFirst n
   . sortByRunSize 
   . countAdjacentRuns
   . sortWords
   . convertIntoLowercase  
   . breakIntoWords

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : (myMap f xs)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ (concat' xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = x `f` (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (z `f` x) xs

sum'' = foldr' (+) 0
concat'' = foldr' (++) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs
-- filter' f (x:xs) = if f x then x : filter' f xs
--                           else filter' f xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort greater
  where 
    smaller = filter' (<= x) xs
    greater = filter' (> x) xs

units :: [String]
units = ["zero", "one", "two", "three", "four", "five", 
         "six", "seven", "eight", "nine", "ten"]
convert1 :: Int -> String
convert1 n = units !! n

teens :: [String]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen",
         "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
      
tens :: [String]
tens = ["twenty", "thirty", "fourty", "fifty", "sixty", "seventy",
         "eighty", "ninety"]

convert2 :: Int -> String
convert2 n
  | t == 0          = convert u
  | t == 1          = teens !! u
  | t > 1 && u == 0 = tens !! (t-2)
  | t > 1 && u /= 0 = tens !! (t-2) ++ "-" ++ convert1 u
    where (t, u) = (n `div` 10, n `mod` 10)

convert3 :: Int -> String
convert3 n
  | h == 0    = convert2 n
  | t == 0    = convert1 h ++ " hundred"
  | otherwise = convert1 h ++ " hundred and " ++ convert2 t
  where (h, t) = (n `div` 100, n `mod` 100)

convert6 :: Int -> String
convert6 n
  | m == 0 = convert3 n
  | h == 0 = convert3 m ++ " thoudsand"
  | otherwise = convert3 m ++ " thoudsand" ++ link h ++ convert3 h
  where (m, h) = (n `div` 1000, n `mod` 1000)

link :: Int -> String
link h = if h < 100 then " and " else " "

convert :: Int -> String
convert = convert6

error' :: String -> a

oops :: a
oops = error' "Oops!"