-- HC6T1: Factorial (Recursive)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC6T2: Fibonacci (Recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC6T3: Sum of Elements Using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- HC6T4: Product of Elements Using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Element Exists in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists n (x:xs)
    | n == x    = True
    | otherwise = elementExists n xs

-- HC6T7: List Length (Recursive)
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

-- HC6T9: Map Implementation (Recursive)
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- HC6T10: Digits of a Number (Recursive)
digits :: Int -> [Int]
digits n
    | n < 10    = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

---------------------------------------------------------
-- MAIN FUNCTION (tests all tasks)
---------------------------------------------------------

main :: IO ()
main = do
    putStrLn "HC6T1: Factorial"
    print (factorial 5)

    putStrLn "HC6T2: Fibonacci"
    print (fibonacci 10)

    putStrLn "HC6T3: Sum of Elements Using foldr"
    print (sumList [1,2,3,4,5])

    putStrLn "HC6T4: Product of Elements Using foldl"
    print (productList [1,2,3,4])

    putStrLn "HC6T5: Reverse List (Recursive)"
    print (reverseList [1,2,3,4,5])

    putStrLn "HC6T6: Element Exists in List"
    print (elementExists 3 [1,2,3,4])
    print (elementExists 10 [1,2,3,4])

    putStrLn "HC6T7: List Length"
    print (listLength [10,20,30,40])

    putStrLn "HC6T8: Filter Even Numbers"
    print (filterEven [1..20])

    putStrLn "HC6T9: Map Implementation"
    print (myMap (*2) [1,2,3,4])

    putStrLn "HC6T10: Digits of a Number"
    print (digits 12345)
