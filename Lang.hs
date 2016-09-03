module Lang where


import Data.List
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set

import TypeSystem


data Term
    = Var VarName
    | Abs VarName Term Term
    | Prod VarName Term Term
    | Term `App` Term


instance Show Term where
    show (Var x)      = x
    show (Abs x t n)  = "\\" ++ x ++ ":" ++ parensT t ++ ". " ++ show n
    show (Prod x t n) = if x `Set.member` freeVars n
        then "["  ++ x ++ ":" ++ parensT t ++ "]"  ++ spaceP n
        else parensL t ++ " -> " ++ show n
    show (m `App` n)  = parensL m ++ " " ++ parensR n

parensT (Var t) = show (Var t)
parensT t = "(" ++ show t ++ ")"

spaceP (Prod x t n) | (x `Set.member` freeVars n) = show (Prod x t n)
spaceP n = " " ++ show n

parensL (Abs x t n)  = "(" ++ show (Abs x t n)  ++ ")"
parensL (Prod x t n) = "(" ++ show (Prod x t n) ++ ")"
parensL x = show x

parensR (Var x) = show (Var x)
parensR x = "(" ++ show x ++ ")"


freeVars :: Term -> Set VarName
freeVars (Var x)      = Set.singleton x
freeVars (Abs x t n)  = freeVars t `Set.union` (Set.delete x (freeVars n))
freeVars (Prod x t n) = freeVars t `Set.union` (Set.delete x (freeVars n))
freeVars (m `App` n)  = freeVars m `Set.union` freeVars n


subst :: Term -> (VarName,Term) -> Term
(Var y) `subst` (x,n)
    | (y == x) = n
    | (y /= x) = Var y
(Abs y t m) `subst` (x,n)
    | (y == x) = Abs y (t `subst` (x,n)) m
    | (y /= x) && (y `Set.notMember` freeVars n) 
        = Abs y (t `subst` (x,n)) (m `subst` (x,n))
    | otherwise = let renamed = alphaAvoiding (freeVars n) (y,t,m) 
                  in  renamed `subst` (x,n)
(Prod y t m) `subst` (x,n) = case (Abs y t m) `subst` (x,n) of -- Not sure if
    (Abs y' t' m') -> (Prod y' t' m')                          -- hack or clever
(m1 `App` m2) `subst` (x,n) = (m1 `subst` (x,n)) `App` (m2 `subst` (x,n))


-- Only works on lambda abstractions
alpha :: (VarName,Term,Term) -> Term
alpha (x,t,n) = alphaAvoiding (Set.singleton x) (x,t,n)


-- Only works on lambda abstractions
alphaAvoiding :: Set VarName -> (VarName,Term,Term) -> Term
alphaAvoiding vs (x,t,n) = Abs selectName t (n `subst` (x, Var selectName))
  where
    varNames    = x : "t" : map (\n -> "t" ++ show n) [1..]
    selectName  = fromJust $ find available varNames
    available v = (v `Set.notMember` freeVars n) && 
                  (v `Set.notMember` vs)
