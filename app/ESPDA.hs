{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeApplications #-}

module ESPDA where

import Data.Proxy
import Data.List

-- implementation of an empty stack PDA 
-- the code relies heavily on the use of the list monad to simulate
-- nondeterminism, which is the driving concept behind es-pdas

-- transition function of pda with alphabet a, states s and stack alphabet w
newtype Transition a s w = Transition { runTransition :: (s, Maybe a, w) -> [(s, [w])] } 

-- corresponding pda with start state and stack symbol
type PDA a s w = (s, w, Transition a s w)

allTransitions :: Transition a s w -> a -> s -> [w] -> [(s, [w])]
allTransitions t x q ws = do
    (q', ws') <- epsilonTransitions t q ws 
    consumeChar t x q' ws' 

-- all single steps which consume the character from the current state
consumeChar :: Transition a s w -> a -> s -> [w] -> [(s, [w])]
consumeChar _              _ _ []     = []
consumeChar t x q (w:ws) = do 
    (q', ws') <- runTransition t (q, Just x, w)
    return (q', ws' ++ ws)

-- add the current configuration (no transition at all) and recurse
-- to find all possible states reachable without reading a character
epsilonTransitions :: Transition a s w -> s -> [w] -> [(s, [w])]
epsilonTransitions _              q []     = return (q, [])
epsilonTransitions t q (w:ws) = (q, w:ws) : do 
    (q1, ws1) <- runTransition t (q, Nothing, w)
    let stack = ws1 ++ ws
    epsilonTransitions t q1 stack

-- given a pda and input word [a], return all
-- possible (state, stack) combinations possible
evaluate :: PDA a s w -> [a] -> [(s, [w])]
evaluate (s0, w0, t) = concatMap allEs . foldl ts [(s0, [w0])]
    where allTs x (q, ws) = allTransitions t x q ws
          allEs   (q, ws) = epsilonTransitions t q ws
          ts acc x        = concatMap (allTs x) acc 

accepts :: PDA a s w -> [a] -> Bool
accepts pda as = any (null . snd) $ evaluate pda as

-- working alphabet for a single state PDA constructed from an existing PDA
data WA s w = S | X s w s deriving (Eq, Show)

class Finite a where
    enumerate :: [a] 
    size :: Proxy a -> Integer -- finite size

subsetsOfLength :: Finite a => Integer -> [[a]]
subsetsOfLength 0 = [[]]
subsetsOfLength n = [ x:xs | x <- enumerate, xs <- subsetsOfLength (n-1) ]

subsets :: forall a. Finite a => [[a]] 
subsets = subsetsOfLength $ size @a Proxy 

singleState :: (Eq s, Finite s) => PDA a s w  -> PDA a () (WA s w)
singleState pda = ((), S, singleStateT pda)

singleStateT :: (Eq s, Finite s) => PDA a s w -> Transition a () (WA s w)
singleStateT (q0, w0, t) = Transition $ \case 
    -- for every state q we add a rule () eps S -> () X(q0, w0, q)
    ((), Nothing, S) -> map (\q -> ((), [X q0 w0 q])) enumerate
    ((), x, X p0 a0 pl) -> do 
            (p1, as) <- runTransition t (p0, x, a0) 
            newTs as p1 pl 
    _ -> [] 

newTs :: (Eq s, Finite s)
      => [w] -- as 
      -> s   -- p1
      -> s   -- pl
      -> [((), [WA s w])]
-- need to be careful here: if p1 == pl, then we do want a rule zuX(p1, A, pl) -> z
-- but if p1 != pl, then such a rule isn't allowed
newTs [] p1 pl = if (p1 == pl) then [((), [])] else []
newTs as p1 pl = do
    -- p2 .. p(l-1)
    ps' <- subsetsOfLength (fromIntegral (length as) - 1)
    -- ps = p1 .. pl
    let ps = p1 : ps' ++ [pl]
    return $ ((), zipWith3 X ps as (tail ps))
