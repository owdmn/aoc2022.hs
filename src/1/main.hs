import GHC.Base
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
accum (x:xs) | x == [] = accum xs
             | otherwise = (sum $ read <$> takeWhile p (x:xs)) : (accum $ (dropWhile p (x:xs)))
             where p = not . eqString ""

