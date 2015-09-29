module Heuristic.LBCS (lbcs) where

import           Heuristic       (InfinityPool, PartialSchedule (..), Pool (..))
import           Problem         (Cost, Ins, Problem, Schedule, Time, cu, nTask,
                                  nType, qcharge, refTime)

import           Control.DeepSeq (NFData (..))
import           Data.List       (minimumBy)
import           Data.Ord        (comparing)
import qualified Data.Vector     as Vec

data CPartial pl = CPar { _pool        :: pl
                        , _locations   :: Vec.Vector Ins
                        , _budget      :: Cost
                        , _usedBudget  :: Cost
                        , _remainWork  :: Time
                        , _usedTime    :: Time
                        , _lastFT      :: Time
                        , _finishTimes :: Vec.Vector Time}

instance (NFData pl)=>NFData (CPartial pl) where
  rnf (CPar a b c d e f g h) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  putTask p s t i = s { _pool = pl'
                      , _locations = _locations s Vec.// [(t, i)]
                      , _usedBudget = _usedBudget s + c
                      , _remainWork = _remainWork s - refTime p t
                      , _usedTime = if _usedTime s < ft then ft else _usedTime s
                      , _lastFT = ft
                      , _finishTimes = _finishTimes s Vec.// [(t, ft)]}
    where (_, ft, pl') = allocIns p s t i
          c = cost pl' i - cost (_pool s) i

  sortSchedule p ss =
    let ub_lowest = minimum . map _usedBudget $ ss
        ub_highest = maximum . map _usedBudget $ ss
        ft_best = minimum . map _lastFT $ ss
        ft_worst = maximum . map _lastFT $ ss
        rw = _remainWork $ head ss
        rcb = minimum [qcharge p (ct, 0, rw / cu p ct)|ct<-[0..nType p-1]]
        b = _budget $ head ss
        r = if b <= rcb + ub_lowest then 1 else
              if b > rcb + ub_highest then 0 else
                (rcb + ub_highest - b) / (ub_highest - ub_lowest)
        _worthiness CPar { _usedBudget=ub, _lastFT=ft} =
          let cr = (ub - ub_lowest) / (ub_highest - ub_lowest)
              tr = (ft - ft_best) / (ft_worst - ft_best)
          in (cr * r + tr * (1-r), tr, cr)
    in [minimumBy (comparing _worthiness) ss]

empty::Pool pl=>Problem->Double->CPartial pl
empty p b = CPar (prepare p) (Vec.replicate (nTask p) 0)
              b 0 rw 0 0 (Vec.replicate (nTask p) 0)
  where rw = sum . map (refTime p) $ [0..nTask p-1]

lbcs::Problem->Double->Schedule
lbcs p b = head . schedule p 1 $ (empty p b :: CPartial InfinityPool)
