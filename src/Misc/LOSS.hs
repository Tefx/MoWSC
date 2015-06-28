module Misc.LOSS (loss2, loss3) where

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

import           Debug.Trace     (traceShow)

inf::Double
inf=1e42

type Weights = Map.Map (Int, Int) Double

data LSchedule = LSchedule { _orders    :: Orders
                           , _locations :: Vec.Vector Ins
                           , _objs      :: [Double]} deriving (Show)

l2Schedule::Problem->Orders->[Ins]->Schedule
l2Schedule p o locs =
  let inss = map head . group . sort $ locs
      index = IntMap.fromList $ zip inss [0..]
      _t2i = map ((IntMap.!) index) $ locs
      _i2t = map (insType p) inss
  in Schedule o (Vec.fromList _t2i) (Vec.fromList _i2t)

schedule2L::Problem->Schedule->LSchedule
schedule2L p s@(Schedule o t2i i2t) =
  let locs = Vec.map (\i->i*nType p+(i2t Vec.! i)) t2i
  in LSchedule { _orders = o
               , _locations = locs
               , _objs = computeObjs s}

newInsWithType::Problem->IntSet.IntSet->InsType->Ins
newInsWithType p s t = if IntSet.member t s
                       then newInsWithType p s $ t + nType p
                       else t

availIns::Problem->LSchedule->[Ins]
availIns p s =
  let existIns = IntSet.fromList . Vec.toList $ _locations s
      newIns = map (newInsWithType p existIns) [0..nType p-1]
  in newIns ++ IntSet.toList existIns

insType::Problem->Ins->InsType
insType p i = i `rem` nType p

calWeightAfterReassign::Problem->LSchedule->(Task, Ins)->Double
calWeightAfterReassign p (LSchedule o l [m, c]) (t, i')
  | l Vec.! t == i' = -1
  | otherwise = let l' = l Vec.// [(t, i')]
                    [m', c'] = computeObjs . l2Schedule p o $ Vec.toList l'
                in if c' >= c then -1
                   else (m' - m) / (c - c')

calWeights::Problem->LSchedule->Map.Map (Int, Int) Double
calWeights p s =
  Map.fromList . filter ((>=0) . snd) $
  [((t,i), calWeightAfterReassign p s (t,i))|t<-[0..nTask p-1],i<-availIns p s]

findReassign::Weights->Maybe (Int, Int)
findReassign m
  | Map.null m = Nothing
  | otherwise = Just . fst . minimumBy (comparing snd) $ Map.toList m

tryReAssign_LOSS2::Problem->LSchedule->Weights->Maybe (LSchedule, Weights)
tryReAssign_LOSS2 p s w = case findReassign w of
  Nothing -> Nothing
  Just move -> let l' = _locations s Vec.// [move]
                   s' = s { _locations = l'
                          , _objs = computeObjs .
                                    l2Schedule p (_orders s) $
                                    Vec.toList l'}
                   w' = Map.delete move w
               in Just (s', w')

tryReAssign_LOSS3::Problem->LSchedule->Weights->Maybe (LSchedule, Weights)
tryReAssign_LOSS3 p s w = case findReassign w of
  Nothing -> Nothing
  Just move -> let l' = _locations s Vec.// [move]
                   s' = s { _locations = l'
                          , _objs = computeObjs .
                                    l2Schedule p (_orders s) $
                                    Vec.toList l'}
                   w' = calWeights p s
               in Just (s', w')

runLOSS::Problem->Weights->Double->
         (Problem->LSchedule->Weights->Maybe (LSchedule, Weights))->
         LSchedule->LSchedule
runLOSS p w b reassigner s = case reassigner p s w of
  Nothing -> s
  Just (s', w') -> if _objs s' !! 1 > b
                   then runLOSS p w' b reassigner s'
                   else s'

loss2::Problem->Double->Schedule
loss2 p b =
  let s = schedule2L p $ heft p
      (LSchedule o l _) = runLOSS p (calWeights p s) b tryReAssign_LOSS2 s
  in l2Schedule p o $ Vec.toList l

loss3::Problem->Double->Schedule
loss3 p b =
  let s = schedule2L p $ heft p
      (LSchedule o l _) = runLOSS p (calWeights p s) b tryReAssign_LOSS3 s
  in l2Schedule p o $ Vec.toList l
