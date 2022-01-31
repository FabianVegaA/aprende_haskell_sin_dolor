import Data.Char (toUpper)

main :: IO ()
main = do
  putStr "Say your name: "
  name <- getLine
  putStrLn ("\nHello, " ++ name ++ "!")
  putStrLn "Welcome to the dark side of programming!"

describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

iterateList :: (a -> b) -> [a] -> [b]
iterateList _ [] = []
iterateList f (x : xs) = f x : iterateList f xs

(-+) :: Int -> Int -> [Int]
(-+) a b = [a - b, a + b]

volumeCyllinder :: Float -> Float -> String
volumeCyllinder height diameter =
  let radius = diameter / 2
      base = pi * (radius ^ 2)
   in "The volume of the cyllinder is " ++ show (base * height)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter cond values = [value | value <- values, cond value]

mySum :: (Num a) => [a] -> a
mySum = foldl1 (+)