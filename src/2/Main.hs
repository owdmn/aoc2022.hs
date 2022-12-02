data RPS = R | P | S
    deriving (Eq, Enum, Show)
   
data Result = Lose | Tie | Win
    deriving (Eq, Enum, Show)

-- decide who wins, opponent goes first
defeats :: RPS -> RPS -> Result
defeats P R = Lose
defeats R P = Win
defeats S P = Lose
defeats P S = Win
defeats R S = Lose
defeats S R = Win
defeats _ _ = Tie

shape :: (RPS, RPS) -> Int
shape x = 1 + fromEnum (snd x)

result :: (RPS, RPS) -> Int
result x = 3 * (fromEnum $ defeats (fst x) (snd x))

parse :: String -> (RPS, RPS)
parse x = (p (x!!0), p (x!!2))
    where 
        p 'A' = R
        p 'X' = R
        p 'B' = P
        p 'Y' = P
        p 'C' = S
        p 'Z' = S
        p  _  = undefined

main :: IO ()
main = do
    input <- readFile "src/2/input"
    let d = parse <$> lines input
    putStrLn $ show $ sum $ (shape <$> d) ++ (result <$> d)
    return ()
