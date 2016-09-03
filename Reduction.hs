module Reduction where


import Data.Set(Set)
import qualified Data.Set as Set

import Lang
import TypeSystem


reduce :: Term -> Term
reduce = etaReduce . reduceB


reduceB :: Term -> Term
reduceB (Var x)      = Var x
reduceB (Abs x t n)  = Abs x (reduceB t) (reduceB n)
reduceB (Prod x t n) = Prod x (reduceB t) (reduceB n)
reduceB (m `App` n)  = reduceB m `apply` reduceB n


apply :: Term -> Term -> Term
(Abs x t m) `apply` n = reduceB $ betaReduce (x,m) n
m `apply` n = m `App` n


betaReduce :: (VarName,Term) -> Term -> Term
betaReduce (x,m) n = m `subst` (x,n)


etaReduce :: Term -> Term
etaReduce (Var x)      = Var x
etaReduce (Abs x t n)  = etaReduce' x t (etaReduce n)
  where
    etaReduce' x t (m `App` Var y)
        | (x == y) && (x `Set.notMember` freeVars m) = m
    etaReduce' x t n = Abs x t n
etaReduce (Prod x t n) = Prod x (etaReduce t) (etaReduce n)
etaReduce (m `App` n)  = etaReduce m `App` etaReduce n


instance Eq Term where
    t1 == t2 = (standardize t1 `eeq` standardize t2)
      where
        standardize = canonym . reduce

        (Var x) `eeq` (Var x')             = (x == x')
        (Abs x t n) `eeq` (Abs x' t' n')   = (x == x') && (t == t') && (n == n')
        (Prod x t n) `eeq` (Prod x' t' n') = (x == x') && (t == t') && (n == n')
        (m `App` n) `eeq` (m' `App` n')    = (m == m') && (n == n')
        _ `eeq` __ = False


canonym :: Term -> Term
canonym = canonym' . prep
  where
    prep (Var x)      = Var x
    prep (Abs x t n)  = Abs ("k" ++ x) (prep t) 
                        (prep $ n `subst` (x, Var ("k" ++ x)))
    prep (Prod x t n) = Prod ("k" ++ x) (prep t) 
                        (prep $ n `subst` (x, Var ("k" ++ x)))
    prep (m `App` n)  = prep m `App` prep n

    canonym' (Var x)      = Var x
    canonym' (Abs x t n)  = alpha (x, canonym t, canonym n)
    canonym' (Prod x t n) = let (Abs x' t' n') = canonym (Abs x t n)  -- Hack
                            in  Prod x' t' n'
    canonym' (m `App` n)  = canonym m `App` canonym n
