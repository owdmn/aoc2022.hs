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

defeats :: RPS -> RPS -> Result
defeats x y
    | csucc x == y = Win
    | cpred x == y = Lose
    | otherwise = Tie

p :: Char -> (RPS, RPS -> RPS)
p c
    | c `elem` "AX" = (R, cpred)
    | c `elem` "BY" = (P, id)
    | c `elem` "CZ" = (S, csucc)
    | otherwise = undefined

parse :: String -> (RPS, RPS)
parse x = (fst $ p.head $ x, fst $ p (x!!2))

parse' :: String -> (RPS, RPS)
parse' x = (fst $ p.head $ x, (snd $ p (x!!2)) $ (fst $ p (head x)))

score :: (RPS, RPS) -> Int
score x = 1 + fromEnum (snd x) + 3 * fromEnum (uncurry defeats x)

main :: IO ()
main = do
    input <- readFile "src/2/input"
    print $ sum $ score . parse  <$> lines input
    print $ sum $ score . parse' <$> lines input
    return ()
