twoSum :: Int -> [Int] -> (Int, Int)
twoSum target values = head [(x, y) | x <- values, y <- values, x + y == target]

partOne :: String -> Int
partOne input =
  let (x, y) = twoSum 2020 $ map read $ lines input 
   in x * y

threeSum :: Int -> [Int] -> (Int, Int, Int)
threeSum target values = head [(x, y, z) | x <- values, y <- values, z <- values, x + y + z == target]

partTwo :: String -> Int
partTwo input = 
  let (x, y, z) = threeSum 2020 $ map read $ lines input
   in x * y * z

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ partOne input
  print $ partTwo input
