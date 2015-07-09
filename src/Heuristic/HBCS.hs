module Heuristic.HBCS (hbcs) where

import           Heuristic
import           Heuristic.Cheap (cheap)
import           Problem

import           Data.List       (maximumBy, minimumBy, sortBy)
import           Data.Ord        (comparing)
import           Data.Vector     (Vector, (//))
import qualified Data.Vector     as Vec

data CPartial pl = CPar { _pool        :: pl --Pool
                        , _locations   :: Vector Ins
                          -- State Information
                        , _rb          :: Cost
                        , _rcb         :: Cost
                        , _lastFT      :: Time
                        , _lastC       :: Cost
                        , _finishTimes :: Vector Time}

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

  sortSchedule _ ss =  (:[]) . _update rcb .
                       maximumBy (comparing _worthiness) $ ss
    where c_lowest = minimum . map _lastC $ ss
          c_highest = maximum . map _lastC $ ss
          CPar {_lastFT=ft_best, _lastC=c_best} = minimumBy (comparing _lastFT) ss
          CPar {_lastFT=ft_worst} = maximumBy (comparing _lastFT) ss
          rcb = (_rcb . head $ ss) - c_lowest
          _worthiness CPar {_rb=rb, _lastFT=ft, _lastC=c}
            | c > c_best || c > rb - rcb = _ninf
            | otherwise = let tr = (ft_worst - ft) / (ft_worst - ft_best)
                              cr = (c_best - c) / (c_highest - c_lowest)
                          in cr * rcb / rb + tr

empty::Pool pl=>Problem->Double->CPartial pl
empty p b = CPar (prepare p) (Vec.replicate (nTask p) 0)
            b rcb 0 0 (Vec.replicate (nTask p) 0)
  where cost_Cheapest = (!!1) . calObjs p . cheap $ p
        rcb = cost_Cheapest

hbcs::Problem->Double->Schedule
hbcs p b = head . schedule p 1 $ (empty p b ::CPartial InfinityPool)

_ninf::Double
_ninf = -1e42

_update::Double->CPartial pl->CPartial pl
_update rcb s = s {_rcb = rcb, _rb = _rb s - _lastC s}
