module Heuristic.LOSS (loss3) where

import           Heuristic.HEFT  (heft)
import           Problem         (Ins, InsType, Orders, Problem, Schedule (..),
                                  Task, nTask, nType)
import           Problem.Foreign (computeObjs)

import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet
import           Data.List       (group, minimumBy, sort)
import qualified Data.Map        as Map
import           Data.Ord        (comparing)
import qualified Data.Vector     as Vec

data LSchedule = LSchedule { _orders    :: Orders
                           , _locations :: Vec.Vector Ins
                           , _hosts     :: IntMap.IntMap IntSet.IntSet
                           , _objs      :: [Double]}

moveTask::Task->Ins->LSchedule->LSchedule
moveTask t i' s =
  let i0 = _locations s Vec.! t
      h' = IntSet.insert t $ IntMap.findWithDefault IntSet.empty i' $ _hosts s
      hs = IntMap.insert i' h'.
           IntMap.adjust (IntSet.delete t) i0 $ _hosts s
  in if IntSet.null $ hs IntMap.! i0
     then s { _hosts = IntMap.delete i0 hs}
     else s { _hosts = hs}

getInsType::Problem->Ins->InsType
getInsType p i = i `mod` nType p

newInsByType::Problem->LSchedule->InsType->Ins
newInsByType p s t = if IntMap.member t $ _hosts s
                     then newInsByType p s $ t + nType p
                     else t

tidySchedule::Problem->[Task]->[Ins]->Schedule
tidySchedule p order locs =
  let inss = map head . group . sort $ locs
      index = Map.fromList $ zip inss [0..]
      _t2i = map ((Map.!) index) $ locs
      _i2t = map (getInsType p) inss
  in Schedule order (Vec.fromList _t2i) (Vec.fromList _i2t)

reassignTask::Problem->LSchedule->Task->Ins->LSchedule
reassignTask p s t i' =
  let locs' = _locations s Vec.// [(t, i')]
  in moveTask t i' $ s { _locations = locs'
                       , _objs = computeObjs .
                                 tidySchedule p (_orders s) $
                                 Vec.toList locs'}

getLOSS::LSchedule->LSchedule->Double
getLOSS (LSchedule {_objs=[m0, c0]})
        (LSchedule {_objs=[m1, c1]})
  | c1 >= c0 = 0
  | otherwise = (m1 - m0) / (c0 - c1)

tryLOSS2forTask::Problem->Task->LSchedule->LSchedule
tryLOSS2forTask p t s =
  let avail_ins = (IntMap.keys $ _hosts s) ++ map (newInsByType p s) [0..nType p-1]
      hs = filter ((>0) . getLOSS s) $ map (reassignTask p s t) avail_ins
  in if null hs then s else minimumBy (comparing $ getLOSS s) hs

tryLOSS2::Problem->Double->LSchedule->LSchedule
tryLOSS2 p b s = foldr (tryLOSS2forTask p) s [0..nTask p-1]

schedule2L::Problem->Schedule->LSchedule
schedule2L p s@(Schedule o t2i i2t) =
  let locs = Vec.map (\i->i*nType p+(i2t Vec.! i)) t2i
  in LSchedule { _orders = o
               , _locations = locs
               , _hosts = foldr insertTask IntMap.empty .
                          zip [0..] $ Vec.toList locs
               , _objs = computeObjs s}

insertTask::(Task,Ins)->IntMap.IntMap IntSet.IntSet->IntMap.IntMap IntSet.IntSet
insertTask (t, i) m = let h' = IntSet.insert t $
                               IntMap.findWithDefault IntSet.empty i m
                      in IntMap.insert i h' m

loss3::Problem->Double->Schedule
loss3 p b = let s' = tryLOSS2 p b .  schedule2L p $ heft p
            in tidySchedule p (_orders s') (Vec.toList $ _locations s')
