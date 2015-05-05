module EA.Chromosome.C2v2 (C2v2) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (..), Task, nIns, nTask, nType)
import           Utils.Random          (chooseWithP, doWithProb, randPos,
                                        randSplit, rouletteSelect)

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

splitHost::[Task]->Int->InsHost->(InsHost, InsHost)
splitHost o p (IH t ts) = let (o0, o1) = splitAt p o
                              ts0 = filter (flip IntSet.member ts) o0
                              ts1 = filter (flip IntSet.member ts) o1
                          in ((IH t $ IntSet.fromList ts0),
                              (IH t $ IntSet.fromList ts1))

data C2v2 = C2v2 { _order :: Orders
                 , _inss  :: Vec.Vector InsHost}


instance Chromosome C2v2 where
  repMode _ = (1, 1)

  mutate p _ (C2v2 o is) = do
    f <- chooseWithP [1/2, 1/2] [mutateMerge, mutateSplit o]
    is' <- f is >>= mutateType p
    o' <- mutateOrder p o
    return $ C2v2 o' is'

  encode p (Schedule _o _t2i _i2t) = C2v2 _o is
    where is = Vec.filter (not . IntSet.null . _tasks) $
               foldr (_insertTask _t2i)
               (Vec.map (flip IH IntSet.empty) _i2t)
               [0..nTask p - 1]
          v = if Vec.length is > nIns p then Vec.length is - nIns p else 0

  decode p (C2v2 _o is) = Schedule _o
                        (Vec.fromList . flip map [0..nTask p-1] $ findIns tidyIS)
                        (Vec.map _type tidyIS)
    where tidyIS = Vec.filter (not . IntSet.null . _tasks) is

mutateMerge::(RandomGen g)=>Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateMerge is = do [p0, p1] <- randPos 2 $ Vec.length is
                    return $ if p0 == p1 then is
                             else Vec.filter notNullIH $
                                  is // [ (p0, mergeHosts (is!p0, is!p1))
                                        , (p1, NullIH)]

mutateSplit::(RandomGen g)=>Orders->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateSplit o is = do pt <- getRandomR (0, length o-1)
                      ph <- getRandomR (0, Vec.length is-1)
                      let (h0, h1) = splitHost o pt $ is ! ph
                      return $ if (IntSet.null $ _tasks h0) ||
                                  (IntSet.null $ _tasks h1)
                               then is
                               else Vec.cons h1 $ is // [(ph, h0)]

mutateType::(RandomGen g)=>Problem->Vec.Vector InsHost->Rand g (Vec.Vector InsHost)
mutateType p is = do po <- getRandomR (0, Vec.length is-1)
                     t' <- getRandomR (0, nType p-1)
                     return $ is // [(po, (is ! po) {_type=t'})]

_insertTask::Vec.Vector Ins->Task->Vec.Vector InsHost->Vec.Vector InsHost
_insertTask _t2i t is = let i = _t2i ! t
                        in is // [(i, insertTask t $ is ! i)]

insertTask::Task->InsHost->InsHost
insertTask t (IH _t _s) = IH _t $ IntSet.insert t _s

findIns::Vec.Vector InsHost->Task->Int
findIns is t = fromJust $ Vec.findIndex (IntSet.member t . _tasks) is
