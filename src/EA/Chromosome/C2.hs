{-# LANGUAGE TupleSections #-}

module EA.Chromosome.C2 (C2) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           Problem
import           Utils.Random          (chooseWithP, doWithProb, randPos,
                                        randSplit)

import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import           Data.Functor          ((<$>))
import           Data.Maybe            (fromJust)
import           Data.Ord              (comparing)
import           Data.Set              (Set, insert, member, notMember, union,
                                        (\\))
import qualified Data.Set              as Set
import           Data.Vector           (Vector, (!), (//))
import qualified Data.Vector           as Vec

data C2 = C2 { _order :: Orders
             , _inss  :: Vector InsHost}

instance Chromosome C2 where
  repMode _ = (1, 1)

  mutate p _ i = do
    let prob = (1.0/) . fromIntegral . Vec.length . _inss $ i
    f <- chooseWithP [0.25, 0.25, 0.5] [ mutateSplit p prob (_order i)
                                       , mutateMerge p prob
                                       , mutateType p prob]
    is' <- f $ _inss i
    o' <- mutateOrder p $ _order i
    return $ C2 o' is'

  encode p (Schedule _o _t2i _i2t) = C2 _o is
    where is = foldr (_insertTask _t2i)
               (Vec.map (flip IHost Set.empty) _i2t)
               [0..nTask p - 1]
          v = if Vec.length is > nIns p then Vec.length is - nIns p else 0

  decode p (C2 _o is) = Schedule _o
                        (Vec.fromList . flip map [0..nTask p-1] $ findIns tidyIS)
                        (Vec.map _type tidyIS)
    where tidyIS = Vec.filter (not . Set.null . _tasks) is

data InsHost = IHost { _type  :: InsType
                     , _tasks :: Set Task}

mergeHosts::Vector InsHost->InsHost
mergeHosts is = let t = _type $ Vec.maximumBy (comparing $ Set.size . _tasks) is
                in IHost t . Vec.foldr union Set.empty . Vec.map _tasks $ is

splitHost::[Task]->InsHost->[Int]->[InsHost]
splitHost o i@(IHost t ts) [p0, p1]
  | p0 == p1 = [i]
  | otherwise = let s' = Set.fromList . take (p1 - p0 + 1) . drop p0
                         $ filter (flip member ts) o
                in [IHost t $ ts \\ s', IHost t s']

updateType::InsHost->InsType->InsHost
updateType (IHost _ ts) t = IHost t ts

updateTask::InsHost->Set Task->InsHost
updateTask (IHost t _) ts = IHost t ts

insertTask::Task->InsHost->InsHost
insertTask t (IHost _t _s) = IHost _t $ insert t _s

mutateMerge::RandomGen g=>Problem->
             Double->Vector InsHost->Rand g (Vector InsHost)
mutateMerge _ prob is = do (m, r) <- randSplit (prob * 2) is
                           return . Vec.snoc r $ mergeHosts m

mutateSplit::RandomGen g=>Problem->
             Double->[Task]->Vector InsHost->Rand g (Vector InsHost)
mutateSplit _ prob o is =
  let f i = Vec.fromList . splitHost o i <$> (randPos 2 . Set.size $ _tasks i)
  in Vec.foldr (Vec.++) Vec.empty <$>
     (flip Vec.mapM is $
      join . doWithProb prob f (return . Vec.singleton))

mutateType::RandomGen g=>Problem->
             Double->Vector InsHost->Rand g (Vector InsHost)
mutateType p prob is = let f i = updateType i <$> getRandomR (0, nType p-1)
                       in flip Vec.mapM is $ join . doWithProb prob f return

findIns::Vector InsHost->Task->Int
findIns is t = fromJust $ Vec.findIndex (member t . _tasks) is

crossoverIns::RandomGen g=>Problem->
              Orders->(Vector InsHost, Vector InsHost)->Rand g (Vector InsHost)
crossoverIns p o (is0, is1) = do
  ts <- Set.fromList . flip take o <$> getRandomR (0, nTask p-1)
  let sub_is0 = Vec.filter (not . Set.null . _tasks) .
                flip Vec.map is0 $
                \i->updateTask i $ Set.filter (flip member ts) (_tasks i)
      sub_is1 = Vec.filter (not . Set.null . _tasks) .
                flip Vec.map is1 $
                \i->updateTask i $ Set.filter (flip notMember ts) (_tasks i)
  return $ (Vec.++) sub_is0 sub_is1

_insertTask::Vector Ins->Task->Vector InsHost->Vector InsHost
_insertTask _t2i t is = let i = _t2i ! t
                        in is // [(i, insertTask t $ is ! i)]
