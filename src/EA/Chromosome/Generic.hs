module EA.Chromosome.Generic ( mutateOrder, crossoverOrder
                             , onePointCrossover, twoPointCrossover) where

import           Problem              (Orders, Problem (..), Task, inp, ins,
                                       nTask)
import           Utils.Random         (randPos)

import           Control.Monad.Random (Rand, RandomGen, getRandomR)
import qualified Data.HashSet         as HashSet
import           Data.Vector          ((!), (//))
import qualified Data.Vector          as Vec
import qualified Data.Vector.Unboxed  as VU


mutateOrder::RandomGen g=>Problem->Orders->Rand g Orders
mutateOrder p s = do l <- getRandomR (0, nTask p - 1)
                     let s' = VU.fromList s
                         t0 = (VU.!) s' l
                         si = findNonPred p s' t0 l
                         ei = findNonSucc p s' t0 l
                     l' <- getRandomR (si, ei)
                     return $! moveTask t0 l l' s

crossoverOrder::RandomGen g=>Problem->Orders->Orders->Rand g [Orders]
crossoverOrder p s0 s1 = do
  l <- getRandomR (0, nTask p - 1)
  let sf0 = take l s0
      sf1 = take l s1
      set0 = HashSet.fromList sf0
      set1 = HashSet.fromList sf1
      s0' = sf0 ++ filter (not . flip HashSet.member set0) s1
      s1' = sf1 ++ filter (not . flip HashSet.member set1) s0
  return $ [s0', s1']

_removeItem::Int->[Task]->[Task]
_removeItem 0 (x:xs) = xs
_removeItem i (x:xs) = x:_removeItem (i-1) xs

_insertItem::Task->Int->[Task]->[Task]
_insertItem t 0 xs = t:xs
_insertItem t i (x:xs) = x:_insertItem t (i-1) xs

moveTask::Task->Int->Int->[Task]->[Task]
moveTask t i j s = _insertItem t j $! _removeItem i s

findNonSucc::Problem->VU.Vector Task->Task->Task->Task
findNonSucc p s task cur
  | cur == nTask p = cur-1
  | ins p ((VU.!) s cur) task = cur-1
  | otherwise = findNonSucc p s task (cur+1)

findNonPred::Problem->VU.Vector Task->Task->Task->Task
findNonPred p s task cur
  | cur < 0 = cur+1
  | inp p ((VU.!) s cur) task = cur+1
  | otherwise = findNonPred p s task (cur-1)

onePointCrossover::RandomGen g=>Vec.Vector a->Vec.Vector a->Rand g [Vec.Vector a]
onePointCrossover i0 i1 = do
  p <- getRandomR (0, Vec.length i0-1)
  let (x0, y0) = Vec.splitAt p i0
      (x1, y1) = Vec.splitAt p i1
  return $ [(Vec.++) x0 y1, (Vec.++) x1 y0]

twoPointCrossover::RandomGen g=>Vec.Vector a->Vec.Vector a->Rand g [Vec.Vector a]
twoPointCrossover i0 i1 = do
  [p0, p1] <- randPos 2 $ Vec.length i0
  let sub0 = Vec.toList $ Vec.slice p0 (p1-p0) i0
      sub1 = Vec.toList $ Vec.slice p0 (p1-p0) i1
  return $ [(i1//) $ zip [p0..] sub0, (i0//) $ zip [p0..] sub1]
