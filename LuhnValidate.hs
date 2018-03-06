module LuhnValidate where

toDigits :: Integer -> [Integer]
toDigits cc = map (\x -> (toInteger . fromEnum) x - 48) (show cc)


step1OddCC :: [Integer] -> [Integer]
step1OddCC xs@[x]     = xs
step1OddCC (x1:x2:xs) = x1 : x2 * 2 : step1OddCC xs


step1EvenCC :: [Integer] -> [Integer]
step1EvenCC []         = []
step1EvenCC (x1:x2:xs) = x1 * 2 : x2 : step1EvenCC xs


step2 :: [Integer] -> [Integer]
step2 []     = []
step2 (x:xs) = if x > 9 then x - 9 : step2 xs else x : step2 xs


isValid :: [Integer] -> Bool
isValid xs = sum xs `mod` 10 == 0

validate' :: [Integer] -> Bool
validate' ccn = if odd  (length ccn)
                then isValid $ step2 . step1OddCC  $ ccn
                else isValid $ step2 . step1EvenCC $ ccn


validate :: Integer -> Bool
validate cc = validate' $ toDigits cc

main :: IO()
main = do
  putStrLn $ if validate 1234567890123456 then "Valid" else "Invalid"
  putStrLn $ if validate 5555555555554444 then "Valid" else "Invalid"
  putStrLn $ if validate 123              then "Valid" else "Invalid"
