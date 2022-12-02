import Data.List

main :: IO ()
main = do
    input <- readFile "src/1/input"
    let d = accum $ lines input
    putStrLn $ "max calories: " ++ show (maximum d)
    putStrLn $ "top 3 sum: " ++ (show.sum.take 3 $ sortBy (flip compare) d)
    return ()

accum :: [[Char]] -> [Int]
accum [] = []
accum (x:xs) = sumUp (x:xs) 0
    where
        sumUp :: [[Char]] -> Int -> [Int]
        sumUp  []     n = n : []
        sumUp ([]:ys) n = n : accum ys
        sumUp ( y:ys) n = sumUp ys (n + (read y :: Int)) 

