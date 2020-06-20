question8 = foldr (&&) True . map (>= 0)

question81 = and . map (>= 0)

question82 :: [Int] -> Bool
question82 = all (>= 0)

question83 :: [Int] -> Bool
question83 = any (>= 0)

question84 :: [Int] -> Bool
question84 = foldr (\a b -> a >= 0 && b) True

question85 :: [Int] -> Bool
question85 = foldl (\a b -> a && b > 0) True
