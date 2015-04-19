module EA.Fitness ( WithFitness (..)
                  , FitnessAssigner
                  , sortByFit) where

import           MOP         (Objectives)
import           Utils       (With)

import           Data.List   (sortBy)
import           Data.Ord    (comparing)
import qualified Data.Vector as Vec

data WithFitness f c o = WithFit { runWithFit :: With c o
                                 , fitness    :: f}

type FitnessAssigner f c o = Vec.Vector (With c o)->Vec.Vector (WithFitness f c o)

sortByFit::(Objectives o, Ord f)=>
           FitnessAssigner f c o->Vec.Vector (With c o)->Vec.Vector (With c o)
sortByFit fa = Vec.fromList . map runWithFit . sortBy (comparing fitness) . Vec.toList . fa
