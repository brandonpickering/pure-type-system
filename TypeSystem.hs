module TypeSystem where


import Control.Monad

import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set


type SortName = String
type VarName  = String -- Every SortName must be a VarName

-- Probably won't work for infinite numbers of sorts, axioms, or relations.
-- Consider creating a TypeSystem class, of which PTS would be an instance.
-- The class should have functions such as "completeRel", "isSort", etc.
-- Finite and infinite type systems would implement these differently.
data PTS = PTS
    { sorts     :: Set SortName
    , axioms    :: Set (VarName,SortName)
    , relations :: Set (SortName,SortName,SortName)
    } deriving Show


completeRel :: PTS -> (SortName,SortName) -> Maybe SortName
completeRel system (s1,s2) = listToMaybe $ do
    (s1',s2',s3) <- Set.toList (relations system)
    guard $ (s1' == s1 && s2' == s2)
    return s3


abrevRel :: (SortName,SortName) -> (SortName,SortName,SortName)
abrevRel (s1,s2) = (s1,s2,s2)


ptsIsValid :: PTS -> Bool
ptsIsValid (PTS ss axs rs) = all validAx (Set.elems axs) && 
                             all validR  (Set.elems rs)  &&
                             validRs
  where
    validAx (c,s) = s `Set.member` ss
    validR (s1,s2,s3) = all (`Set.member` ss) [s1,s2,s3]

    validRs = let lrs = Set.toList rs 
              in  not $ any contRs [(r1,r2) | r1 <- lrs, r2 <- lrs]
    contRs ((s1,s2,s3),(u1,u2,u3)) = (s1 == u1) && (s2 == u2) && (s3 /= u3)
