module EA.Chromosome.C2v3 (C2v3) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (..), Task, nIns, nTask, nType)
import           Utils.Random          (chooseWithP, doWithProb, randPos,
                                        randSplit, rouletteSelect)

import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import qualified Data.IntSet           as IntSet
import           Data.Maybe            (fromJust)
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

data InsHost = IH { _type  :: InsType
                  , _tasks :: IntSet.IntSet}
             | NullIH

notNullIH::InsHost->Bool
notNullIH NullIH = False
notNullIH (IH _ _) = True

mergeHosts::(InsHost, InsHost)->InsHost
mergeHosts (IH t0 ts0, IH t1 ts1) = IH t0 $ IntSet.union ts0 ts1

splitHost::[Task]->(Int, Int)->InsHost->(InsHost, InsHost)
splitHost o (p0, p1) (IH t ts) = let ts0 = IntSet.fromList .
                                           drop p0 . take (p1+1) $
                                           filter (flip IntSet.member ts) o
                                     ts1 = IntSet.filter (not . flip IntSet.member ts0) ts
                                 in (IH t ts0, IH t ts1)

moveTask::Task->InsHost->InsHost->(InsHost, InsHost)
moveTask t (IH t0 ts0) (IH t1 ts1) = (IH t0 $ IntSet.delete t ts0,
                                     IH t1 $ IntSet.insert t ts1)

findHost::Task->Vec.Vector InsHost->Int
findHost t is = fromJust $ Vec.findIndex (IntSet.member t . _tasks) is

data C2v3 = C2v3 { _order :: Orders
                 , _inss  :: Vec.Vector InsHost}

instance Chromosome C2v3 where
  repMode _ = (1, 1)

  mutate p cur (C2v3 o is) = do
    f <- chooseWithP [1/2, 1/2] [mutateMerge, mutateSplit o]
    is' <- (join $ doWithProb 0.9 f return is)
           -- >>= join . doWithProb (1-0.5*cur) (mutateDrift p) return
           >>= join . doWithProb 0.9 (mutateType p) return
    o' <- mutateOrder p o
    return . C2v3 o' $ Vec.filter (not . IntSet.null . _tasks) is'

  encode p (Schedule _o _t2i _i2t) = C2v3 _o is
    where is = Vec.filter (not . IntSet.null . _tasks) $
               foldr (_insertTask _t2i)
               (Vec.map (flip IH IntSet.empty) _i2t)
               [0..nTask p - 1]
          v = if Vec.length is > nIns p then Vec.length is - nIns p else 0

  decode p (C2v3 _o is) = Schedule _o
                          (Vec.fromList $ map (flip findHost is) [0..nTask p-1])
                          (Vec.map _type is)

mutateMerge::(RandomGen g)=>Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateMerge is = do [p0, p1] <- randPos 2 $ Vec.length is
                    return $ if p0 == p1 then is
                             else Vec.filter notNullIH $
                                  is // [ (p0, mergeHosts (is!p0, is!p1))
                                        , (p1, NullIH)]

mutateSplit::(RandomGen g)=>Orders->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateSplit o is = do ph <- getRandomR (0, Vec.length is-1)
                      [p0, p1] <- randPos 2 . IntSet.size . _tasks $ is ! ph
                      let (h0, h1) = splitHost o (p0, p1) $ is ! ph
                      return $ if (IntSet.null $ _tasks h0) ||
                                  (IntSet.null $ _tasks h1)
                               then is
                               else Vec.cons h1 $ is // [(ph, h0)]

mutateType::(RandomGen g)=>Problem->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateType p is = do po <- getRandomR (0, Vec.length is-1)
                     t' <- getRandomR (0, nType p-1)
                     return $ is // [(po, (is ! po) {_type=t'})]

mutateDrift::(RandomGen g)=>Problem->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateDrift p is = do t <- getRandomR (0, nTask p-1)
                      let p0 = findHost t is
                      p1 <- getRandomR (0, Vec.length is - 1)
                      if p0 == p1
                        then return is
                        else let (h0, h1) = moveTask t (is!p0) (is!p1)
                             in return . Vec.filter (not . IntSet.null . _tasks) $
                                is // [(p0, h0), (p1, h1)]

_insertTask::Vec.Vector Ins->Task->Vec.Vector InsHost->Vec.Vector InsHost
_insertTask _t2i t is = let i = _t2i ! t
                        in is // [(i, insertTask t $ is ! i)]

insertTask::Task->InsHost->InsHost
insertTask t (IH _t _s) = IH _t $ IntSet.insert t _s
