{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}


module EA.Utils ( WithFitness, fitness
                , FitnessAssigner
                , sortByFit) where

import           MOP         (Objectives, WithObjs (..))
import           Utils       (With, attached, original)

import           Data.List   (sortBy)
import           Data.Ord    (comparing)
import           Data.Vector ((//))
import qualified Data.Vector as Vec

type WithFitness f o = With f o

fitness::(Ord f)=>WithFitness f o->f
fitness = attached

type FitnessAssigner f = (WithObjs o)=>Vec.Vector o->Vec.Vector (WithFitness f o)

sortByFit::(WithObjs o, Ord f)=>
           FitnessAssigner f->Vec.Vector o->Vec.Vector o
sortByFit fa = Vec.fromList . map original . sortBy (comparing fitness) . Vec.toList . fa
