module TypeCheck where


import Control.Applicative
import Control.Monad
import Control.Monad.State

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

import Lang
import TypeSystem
import Reduction


type Context   = (PTS, Map VarName Term)
type Contexted = StateT Context Maybe


getTSystem :: Contexted PTS
getTSystem = gets fst


getVarTypes :: Contexted (Map VarName Term)
getVarTypes = gets snd


setVarType :: (VarName,Term) -> Contexted ()
setVarType (x,t) = do
    guard =<< isSort =<< checkType t
    guard . (x `Set.notMember`) =<< definedNames
    (ss,vs) <- get 
    put (ss, Map.insert x t vs)
    

checkVarType :: VarName -> Contexted (Maybe Term)
checkVarType x = fmap (Map.lookup x) getVarTypes


getSorts :: Contexted (Set SortName)
getSorts = fmap sorts getTSystem


completeSortRel :: (SortName,SortName) -> Contexted SortName
completeSortRel r' = do
    system <- getTSystem
    Just r <- return $ completeRel system r'
    return r


definedNames :: Contexted (Set VarName)
definedNames = do
    vars  <- fmap (Set.fromList . Map.keys) getVarTypes
    sorts <- getSorts
    return $ vars `Set.union` sorts


isSort :: Term -> Contexted Bool
isSort n = do
    sorts <- fmap Set.toList getSorts
    let tsorts = map Var sorts
    return $ any (== n) tsorts


toSort :: Term -> Contexted SortName
toSort n = do
    guard =<< isSort n
    Var s <- return $ reduce n
    return s


pushContext :: Contexted a -> Contexted a
pushContext k = do
    savedContext <- get
    v <- k
    put savedContext
    return v


checkType :: Term -> Contexted Term
checkType (Var x) = do
    Just t <- checkVarType x
    return t
checkType (Abs x t n) = do
    defNames <- definedNames
    if x `Set.member` defNames
        then let renamed = alphaAvoiding defNames (x,t,n)
             in  checkType renamed
        else do
            b <- pushContext $ do
                setVarType (x,t)
                checkType n
            guard =<< isSort =<< checkType (Prod x t b)
            return (Prod x t b)
checkType (Prod x t n) = do
    defNames <- definedNames
    if x `Set.member` defNames
        then let Abs x' t' n' = alphaAvoiding defNames (x,t,n) -- HACK
             in  checkType (Prod x' t' n')
        else do
            s1 <- toSort =<< checkType t
            s2 <- pushContext $ do
                setVarType (x,t)
                checkType n >>= toSort
            fmap Var $ completeSortRel (s1,s2)
checkType (f `App` a) = do
    (Prod x t n) <- fmap reduce (checkType f)
    t' <- checkType a
    guard (t == t')
    return (n `subst` (x,a))


initialContext :: PTS -> Context
initialContext system = (system, vts)
  where
    vts = Map.fromList [(x,Var s) | (x,s) <- Set.toList (axioms system)]


typeCheck :: PTS -> Term -> Maybe Term
typeCheck system n = fmap (reduce . fst) $ 
                     runStateT (checkType n) (initialContext system)
