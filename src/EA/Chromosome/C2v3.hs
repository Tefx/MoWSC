module EA.Chromosome.C2v3 (C2v3) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (..), Task, nIns, nTask, nType)
import           Utils.Random          (choose, chooseWithP, doWithProb,
                                        randPos, randSplit, rouletteSelect)

import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import           Data.Functor          ((<$>))
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

mergeHosts::(InsHost, InsHost)->InsType->InsHost
mergeHosts (IH t0 ts0, IH t1 ts1) t = IH t $ IntSet.union ts0 ts1

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

typeMutate::(RandomGen g)=>Problem->InsHost->Rand g InsHost
typeMutate p (IH t ts) = flip IH ts <$> getRandomR (0, nType p-1)

mergeMutate::(RandomGen g)=>Problem->(InsHost, InsHost)->Rand g InsHost
mergeMutate p is@(IH t0 ts0, IH t1 ts1) = mergeHosts is <$> choose t0 t1

splitMutate::(RandomGen g)=>Problem->Orders->InsHost->Rand g (InsHost, InsHost)
splitMutate p o h = do [p0, p1] <- randPos 2 .IntSet.size $ _tasks h
                       return $ splitHost o (p0, p1) h

data C2v3 = C2v3 { _order :: Orders
                 , _inss  :: Vec.Vector InsHost}

instance Chromosome C2v3 where
  repMode _ = (1, 1)

  mutate p cur (C2v3 o is) = do
    f <- chooseWithP [1/2, 1/2] [mutateByMerging p, mutateBySplitting p o]
    is' <- (join $ doWithProb 0.95 f return is) >>=
           join . doWithProb 0.95 (mutateInTypes p) return
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

mutateByMerging::(RandomGen g)=>Problem->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateByMerging p is = do [p0, p1] <- randPos 2 $ Vec.length is
                          if p0 == p1
                            then return is
                            else do h' <- mergeMutate p (is!p0, is!p1)
                                    return . Vec.filter notNullIH $
                                      is // [(p0, h'), (p1, NullIH)]

mutateBySplitting::(RandomGen g)=>Problem->Orders->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateBySplitting p o is = do ph <- getRandomR (0, Vec.length is-1)
                              (h0, h1) <- splitMutate p o $ is ! ph
                              return $ if (IntSet.null $ _tasks h0) ||
                                          (IntSet.null $ _tasks h1)
                                       then is
                                       else Vec.cons h1 $ is // [(ph, h0)]

mutateInTypes::(RandomGen g)=>Problem->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateInTypes p is = let pb = 1.0 / (fromIntegral $ Vec.length is)
                     in Vec.mapM (join. doWithProb pb (typeMutate p) return) is

mutateByDriftting::(RandomGen g)=>Problem->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateByDriftting p is = do t <- getRandomR (0, nTask p-1)
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
