{-# LANGUAGE TupleSections #-}

module EA.Chromosome.C3 (C3) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (Schedule), Task, nTask, nType)
import           Utils.Random          (choose, choose, doWithProb, randPos)

import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import           Data.Function         (on)
import           Data.Functor          ((<$>))
import qualified Data.IntSet           as IntSet
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

data Host = Host { _type  :: InsType
                 , _tasks :: IntSet.IntSet}

mergeHosts::(Host, Host)->InsType->Host
mergeHosts (Host t0 ts0, Host t1 ts1) t = Host t $ IntSet.union ts0 ts1

splitHost::[Task]->Host->(Int, Int)->(Host, Host)
splitHost o (Host t ts) (p0, p1) =
  let ts0 = IntSet.fromList . drop p0 . take p1 $
            filter (flip IntSet.member ts) o
      ts1 = IntSet.filter (not . flip IntSet.member ts0) ts
  in (Host t ts0, Host t ts1)

insertTask::Task->Host->Host
insertTask t (Host _t _s) = Host _t $ IntSet.insert t _s

_mergeMutate::(RandomGen g)=>(Host, Host)->Rand g Host
_mergeMutate is@(Host t0 _, Host t1 _) = mergeHosts is <$> choose t0 t1

_splitMutate::(RandomGen g)=>Orders->Host->Rand g (Host, Host)
_splitMutate o h = do [p0, p1] <- (randPos 2 . (+1) . IntSet.size $ _tasks h)
                      return $ splitHost o h (p0, p1)

_typeMutate::(RandomGen g)=>Problem->Host->Rand g Host
_typeMutate p (Host t ts) = flip Host ts <$> getRandomR (0, nType p-1)


data C3 = C3 { _order :: Orders
             , _inss  :: Vec.Vector Host}

instance Chromosome C3 where
  repMode _ = (1, 1)

  mutate p _ (C3 o hs) = do
    mf <- choose mutateByMerging $ mutateBySplitting o
    hs' <- mf hs >>= mutateInTypes p
    o' <- mutateOrder p o
    return $ C3 o' hs'

  encode p (Schedule o t2i i2t) =
    let _insert t is = let i = t2i ! t in is // [(i, insertTask t $ is!i)]
    in C3 o . Vec.filter (not . IntSet.null . _tasks) $
       foldr _insert (Vec.map (flip Host IntSet.empty) i2t) [0..nTask p-1]

  decode p (C3 o hs) =
    let _assignIns i ts = map (,i) . IntSet.toList $ _tasks ts
        i2t = Vec.map _type hs
        t2i = Vec.foldr (flip (//)) (Vec.replicate (nTask p) 0) $
              Vec.imap _assignIns hs
    in Schedule o t2i i2t

mutateByMerging::(RandomGen g)=>Vec.Vector Host->Rand g (Vec.Vector Host)
mutateByMerging hs = do [p0, p1] <- randPos 2 $ Vec.length hs
                        if p0 == p1 then return hs
                          else do h' <- _mergeMutate (hs ! p0, hs ! p1)
                                  return . Vec.ifilter (\i _->i /= p1) $
                                    hs // [(p0, h')]

mutateBySplitting::(RandomGen g)=>Orders->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateBySplitting o hs = do p <- getRandomR (0, Vec.length hs - 1)
                            (h0, h1) <- _splitMutate o $ hs ! p
                            if (IntSet.null $ _tasks h0) ||
                               (IntSet.null $ _tasks h1)
                              then return hs
                              else return . Vec.cons h1 $ hs // [(p, h0)]

mutateInTypes::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateInTypes p hs =
  flip Vec.mapM hs $
  join . doWithProb (1.0 / (fromIntegral $ Vec.length hs)) (_typeMutate p) return
