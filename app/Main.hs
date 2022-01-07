module Main where

import ESPDA
import Data.List

-- a simple pda that accepts the language a^nb^n for all n >= 0
-- we explicity encoded characters A, B, but we could also use
-- data Sigma = Sigma Char and a Finite instance with 
-- enumerate = ['a', 'b'] if we preferred
data Z = P | Q deriving (Eq, Show)
data Sigma = A | B deriving (Eq, Show)
data Gamma = V | T deriving (Eq, Show)

instance Finite Z where
    enumerate = [P, Q]
    size _    = 2

instance Finite Gamma where
    enumerate = [V, T]
    size _    = 2

t, t2 :: (Z, Maybe Sigma, Gamma) -> [(Z, [Gamma])]
t (Q, Nothing,  T) = [(Q, [])]
t (Q, (Just A), T) = [(Q, [V])]
t (Q, (Just A), V) = [(Q, [V, V])]
t (Q, (Just B), V) = [(P, [])]
t (P, (Just B), V) = [(P, [])]
t _                = []

pda = (Q, T, Transition t)

-- enumerating things for testing purposes
allOfLength :: Int -> [a] -> [[a]]
allOfLength 0 xs = [[]]
allOfLength _ [] = []
allOfLength n xs = [ x:xs' | x <- xs, xs' <- allOfLength (n-1) xs]

allUpToLength :: Int -> [a] -> [[a]]
allUpToLength n xs = concatMap (flip allOfLength xs) [0..n]

allWs :: [WA Z Gamma]
allWs = S : do 
    p <- enumerate
    a <- enumerate
    q <- enumerate
    return $ X p a q

allXs :: [(Maybe Sigma, WA Z Gamma)]
allXs = do
    w <- allWs
    [(Just A, w), (Just B, w), (Nothing, w)]

main :: IO ()
main = let tests = allUpToLength 10 [A, B]
           pda'@(_, _, Transition f) = singleState pda
           accepted = filter (accepts pda') tests
           rules = map (\(x, w) -> ((x, w), f ((), x, w))) allXs
       in  print accepted 

printRules = mapM_ (putStrLn . pp) . nub . filter (not . null . snd)

-- awfully written pretty printing for the rules of the generated single
-- state es-pda (for debugging)
pp :: (Show a, Show w, Show s) => ((Maybe a, WA s w), [((), [WA s w])]) -> String
pp ((x, y), ws) = 
    let char = case x of 
            Nothing  -> "ε" 
            (Just a) -> show a
        top w = case w of
            S         -> "S"
            (X q w p) -> "X(" ++ show q 
                              ++ ", " ++ show w 
                              ++ ", " ++ show p 
                              ++ ")"
        oneStack ys = ('z':) . foldr (++) "" . map top . snd $ ys
        list = foldr (++) "" . intersperse ", " . map oneStack $ ws
    in "z" ++ char ++ top y ++ " -> " ++ list

