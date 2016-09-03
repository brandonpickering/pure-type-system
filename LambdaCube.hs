module LambdaCube where


import Data.Set(Set)
import qualified Data.Set as Set

import TypeSystem


lambdaCubeCoord :: (Int,Int,Int) -> PTS
lambdaCubeCoord = lambdaCube . relsByLCCoord


lambdaCube :: [(SortName,SortName)] -> PTS
lambdaCube rels = PTS
    { sorts     = Set.fromList [sort0,sort1]
    , axioms    = Set.fromList [(sort0,sort1)]
    , relations = Set.fromList $ map abrevRel rels
    }

sort0, sort1 :: String
sort0 = "*"
sort1 = "*1"


relsByLCCoord :: (Int,Int,Int) -> [(SortName,SortName)]
relsByLCCoord (x,y,z) = relsByBC ((x /= 0),(y /= 0),(z /= 0))
  where
    relsByBC (x,y,z) = [r | (True,r) <- ([True,x,y,z] `zip` rels)]
    rels = [(sort0,sort0), (sort0,sort1), (sort1,sort0), (sort1,sort1)]


simplyTypedLC = lambdaCubeCoord (0,0,0)

logFramework  = lambdaCubeCoord (1,0,0) -- dependent typing
systemF       = lambdaCubeCoord (0,1,0) -- universal quantification over types
omega         = lambdaCubeCoord (0,0,1) -- type constructors

systemF_omega = lambdaCubeCoord (0,1,1)
calculusOfC   = lambdaCubeCoord (1,1,1)
