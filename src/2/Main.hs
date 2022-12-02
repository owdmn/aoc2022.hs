{-# LANGUAGE DeriveAnyClass #-}

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred x
        | x == minBound = maxBound
        | otherwise = pred x
    csucc :: a -> a
    csucc x
        | x == maxBound = minBound
        | otherwise = succ x

data RPS = R | P | S
    deriving (Eq, Enum, Bounded, CyclicEnum, Show)

data Result = Lose | Tie | Win
    deriving (Eq, Enum, Show)

-- decide who wins, opponent goes first
defeats :: RPS -> RPS -> Result
defeats x y
    | csucc x == y = Win
    | cpred x == y = Lose
    | otherwise = Tie

score :: (RPS, RPS) -> Int
score x = 1 + fromEnum (snd x) + 3 * fromEnum (uncurry defeats x)

p :: Char -> RPS
p c
    | c `elem` "AX" = R
    | c `elem` "BY" = P
    | c `elem` "CZ" = S
    | otherwise = undefined

parse :: String -> (RPS, RPS)
parse x = (p (head x), p (x!!2))

parse' :: String -> (RPS, RPS)
parse' x = (p (head x), pickMove (p (head x)) (q (x!!2)))
    where 
        q :: Char -> Result
        q 'X' = Lose
        q 'Y' = Tie
        q 'Z' = Win
        q  _  = undefined

pickMove :: RPS -> Result -> RPS
pickMove x Win = csucc x
pickMove x Lose = cpred x
pickMove x Tie = x

main :: IO ()
main = do
    input <- readFile "src/2/input"
    print $ sum $ score . parse  <$> lines input
    print $ sum $ score . parse' <$> lines input
    return ()
