module Heuristic.HBCS (hbcs) where

import Problem
import Heuristic
import Heuristic.HEFT (heft)
import Heuristic.Cheap (cheap)

import Data.Vector (Vector, (//))
import qualified Data.Vector as Vec
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Ord (comparing)

data CPartial pl = CPar { _pool::pl --Pool
                        , _locations::Vector Ins
                          -- State Information
                        , _rb::Cost
                        , _rcb::Cost
                        , _lastFT::Time
                        , _lastC::Cost
                        , _finishTimes::Vector Time}

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  putTask p s t i = s { _pool = pl'
                      , _lastFT = ft
                      , _lastC = c
                      , _finishTimes = _finishTimes s // [(t, ft)]
                      , _locations = _locations s // [(t, i)]}
    where (_, ft, pl, pl') = allocIns p s t i
          c = cost pl' i - cost pl i

  sortSchedule _ ss =  map (_update rcb) .
                       reverse . sortBy (comparing _worthiness) $ ss
    where c_lowest = minimum . map _lastC $ ss
          c_highest = maximum . map _lastC $ ss
          CPar {_lastFT=ft_best, _lastC=c_best} = minimumBy (comparing _lastFT) ss
          CPar {_lastFT=ft_worst} = maximumBy (comparing _lastFT) ss
          rcb = (_rcb . head $ ss) - c_lowest
          _worthiness CPar {_rb=rb, _lastFT=ft, _lastC=c}
            | c > c_best || c > rb - rcb = _ninf
            | otherwise = let tr = (ft_worst - ft) / (ft_worst - ft_best)
                              cr = (c_best - c) / (c_highest - c_lowest)
                          in cr * rcb / rb * 0.1 + tr

empty::Pool pl=>Problem->Double->CPartial pl
empty p k = CPar (prepare p) (Vec.replicate (nTask p) 0)
            rb rcb 0 0 (Vec.replicate (nTask p) 0)
  where cost_HEFT = (!!1) . calObjs p . heft $ p
        cost_Cheapest = (!!1) . calObjs p . cheap $ p
        rb = cost_Cheapest + k * (cost_HEFT - cost_Cheapest)
        rcb = cost_Cheapest

hbcs::Problem->Double->Schedule
hbcs p k = head . schedule p 1 $ (empty p k ::CPartial InfinityPool)

_ninf::Double
_ninf = -1e42

_update::Double->CPartial pl->CPartial pl
_update rcb s = s {_rcb = rcb, _rb = _rb s - _lastC s}
