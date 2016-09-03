module TypeSystems
( module TypeSystems
, module LambdaCube
) where


import Data.Set(Set)
import qualified Data.Set as Set

import LambdaCube hiding (sort0,sort1)
import TypeSystem


systemX = PTS
    { sorts     = Set.singleton "*"
    , axioms    = Set.singleton ("*","*")
    , relations = Set.singleton $ abrevRel ("*","*")
    }
