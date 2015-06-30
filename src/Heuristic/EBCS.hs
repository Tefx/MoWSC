module Heuristic.EBCS (ebcs) where

import           Heuristic   (InfinityPool, PartialSchedule (..), Pool (..))
import           Problem     (Cost, Ins, Problem, Schedule, Time, cu, nTask,
                              nType, qcharge, refTime)

import           Data.List   (minimumBy)
import           Data.Ord    (comparing)
import qualified Data.Vector as Vec

data CPartial pl = CPar { _pool         :: pl
                        , _locations    :: Vec.Vector Ins
                        , _remainBudget :: Cost
                        , _remainWork   :: Time
                        , _lastFT       :: Time
                        , _lastC        :: Cost
                        , _finishTimes  :: Vec.Vector Time}

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  putTask p s t i = s { _pool = pl'
                      , _lastFT = ft
                      , _lastC = c
                      , _remainWork = _remainWork s - refTime p t
                      , _finishTimes = _finishTimes s Vec.// [(t, ft)]
                      , _locations = _locations s Vec.// [(t, i)]}
    where (_, ft, pl, pl') = allocIns p s t i
          c = cost pl' i - cost pl i

  sortSchedule p ss =
    let _update s = s {_remainBudget = _remainBudget s - _lastC s}
        c_lowest = minimum . map _lastC $ ss
        c_highest = maximum . map _lastC $ ss
        ft_best = minimum . map _lastFT $ ss
        ft_worst = maximum . map _lastFT $ ss
        rw = _remainWork $ head ss
        rb = _remainBudget $ head ss
        rcb = minimum [qcharge p (ct, 0, rw / cu p ct)|ct<-[0..nType p-1]]
        r = if rb <= rcb + c_lowest  then 1 else
              if rb > rcb + c_highest then 0 else
                exp $ (rcb + c_lowest - rb) / (c_highest - c_lowest)
        _worthiness CPar { _lastFT=ft, _lastC=c} =
          let cr = (c - c_lowest) / (c_highest - c_lowest)
              tr = (ft - ft_best) / (ft_worst - ft_best)
          in (cr * r + tr * (1-r), tr, cr)
    in (:[]) . _update . minimumBy (comparing _worthiness) $ ss

empty::Pool pl=>Problem->Double->CPartial pl
empty p b = CPar (prepare p) (Vec.replicate (nTask p) 0)
              b rw 0 0 (Vec.replicate (nTask p) 0)
  where rw = sum . map (refTime p) $ [0..nTask p-1]

ebcs::Problem->Double->Schedule
ebcs p b = head . schedule p 1 $ (empty p b :: CPartial InfinityPool)
