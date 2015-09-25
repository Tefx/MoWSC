module Heuristic.HEFT (heft) where

import Problem
import Heuristic

import Data.Vector (Vector, (//))
import qualified Data.Vector as Vec
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.DeepSeq (NFData (..))

data CPartial pl = CPar { _pool::pl --Pool
                          -- State information
                        , _lastFT::Double -- A metric for decision
                        , _lastC::Double
                        , _finishTimes::Vector Time
                        , _locations::Vector Ins}

instance (NFData pl)=>NFData (CPartial pl) where
  rnf (CPar a b c d e) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  sortSchedule _ ss = sortBy (comparing $ \x->(_lastFT x, _lastC x)) $ ss

  putTask p s t i = s { _pool = pl'
                      , _lastFT = ft
                      , _lastC = cost pl' i - cost pl i
                      , _finishTimes = _finishTimes s // [(t, ft)]
                      , _locations = _locations s // [(t, i)]}
          where (_, ft, pl, pl') = allocIns p s t i


empty::Pool pl=>Problem->CPartial pl
empty p = CPar (prepare p) 0 0
          (Vec.replicate (nTask p) 0) (Vec.replicate (nTask p) 0)

heft::Problem->Schedule
heft p = head . schedule p 1 $ (empty p ::CPartial InfinityPool)
