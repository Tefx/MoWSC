module Heuristic.BHEFT (bheft) where

import           Heuristic   (InfinityPool, PartialSchedule (..), Pool (..))
import           Problem     (Cost, Ins, Problem, Schedule, Task, Time, cu,
                              insPrice, nTask, nType, refTime)

import           Data.List   (minimumBy)
import           Data.Ord    (comparing)
import qualified Data.Vector as Vec

data CPartial pl = CPar { _pool         :: pl
                        , _locations    :: Vec.Vector Ins
                        , _finishTimes  :: Vec.Vector Time
                        , _budget       :: Cost
                        , _usedBudget   :: Cost
                        , _meanLeftCost :: Cost
                        , _currentTask  :: Task
                        , _currentCost  :: Cost
                        , _currentTime  :: Time}

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  putTask p s t i =
    let (_, ft, pl, pl') = allocIns p s t i
        -- it = insType pl i
    in s { _pool = pl'
         , _locations = _locations s Vec.// [(t, i)]
         , _finishTimes = _finishTimes s Vec.// [(t, ft)]
         , _currentTask = i
         , _currentCost = cost pl' i - cost pl i
         -- , _currentCost = refTime p t / cu p it / 3600 * insPrice p it
         , _currentTime = ft}

  sortSchedule p ss =
    let s = head ss
        c_cur_mean = meanCostGrid p $ _currentTask s
        c_left = _meanLeftCost s
        sab = _budget s - _usedBudget s - c_left
        af = if sab < 0 then 0 else c_cur_mean / c_left
        ctb = c_cur_mean + sab * af
        s_avail = filter ((<=ctb) . _currentCost) ss
        in (:[]) . _update c_cur_mean $
           if null s_avail then
             if sab >= 0 then minimumBy (comparing _currentTime) ss
             else minimumBy (comparing _currentCost) ss
           else minimumBy (comparing _currentTime) s_avail

_update::Double->CPartial pl->CPartial pl
_update c_m s = s { _usedBudget = _usedBudget s + _currentCost s
                  , _meanLeftCost = _meanLeftCost s - c_m}

meanCostGrid::Problem->Task->Cost
meanCostGrid p task =
  let f t i d = d + refTime p t / cu p i / 3600 * insPrice p i
  in foldr (f task) 0 [0..nType p-1] / fromIntegral (nType p)

empty::Pool pl=>Problem->Double->CPartial pl
empty p b = let c_all = sum $ map (meanCostGrid p) [0..nTask p-1]
            in CPar (prepare p) (Vec.replicate (nTask p) 0)
                    (Vec.replicate (nTask p) 0) b 0 c_all 0 0 0

bheft::Problem->Double->Schedule
bheft p b = head . schedule p 1 $ (empty p b :: CPartial InfinityPool)
