{-# LANGUAGE TupleSections #-}

module EA.Chromosome.C3 (C3) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Foreign (probSelect)
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (Schedule), Task, nTask, nType)
import           Utils.Random          (choose, doWithProb, probApply, randPos,
                                        rouletteSelect)

import           Control.DeepSeq       (NFData (..), force)
import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandom, getRandomR)
import           Data.Function         (on)
import           Data.Functor          ((<$>))
import qualified Data.IntSet           as IntSet
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

import           Debug.Trace           (traceShow)

data Host = Host { _type  :: InsType
                 , _tasks :: IntSet.IntSet}

instance NFData Host where
  rnf (Host _p _t) = rnf _p `seq` rnf _t `seq` ()

mergeHosts::(Host, Host)->InsType->Host
mergeHosts (Host _ ts0, Host _ ts1) t = Host t $ IntSet.union ts0 ts1

splitHost::[Task]->Host->(Int, Int)->[Host]
splitHost o (Host t ts) (p0, p1) =
  let ts0 = IntSet.fromList . drop p0 . take p1 $
            filter (flip IntSet.member ts) o
      ts1 = IntSet.filter (not . flip IntSet.member ts0) ts
  in [Host t ts0, Host t ts1]

insertTask::Task->Host->Host
insertTask t (Host _t _s) = Host _t $ IntSet.insert t _s

_mergeMutate::(RandomGen g)=>(Host, Host)->Rand g Host
_mergeMutate is@(Host t0 _, Host t1 _) = mergeHosts is <$> choose t0 t1

_splitMutate::(RandomGen g)=>Orders->Host->Rand g [Host]
_splitMutate o h = do [p0, p1] <- (randPos 2 . (+1) . IntSet.size $ _tasks h)
                      return $ splitHost o h (p0, p1)

_typeMutate::(RandomGen g)=>Problem->Host->Rand g Host
_typeMutate p (Host t ts) = flip Host ts <$> getRandomR (0, nType p-1)


data C3 = C3 { _order :: Orders
             , _inss  :: Vec.Vector Host}

instance NFData C3 where
  rnf (C3 _o _i) = rnf _o `seq` rnf _i `seq` ()

instance Chromosome C3 where
  repMode _ = (1, 1)

  mutate p _ (C3 o hs) = do
    mf <- choose mutateByMerging $ mutateBySplitting o
    hs' <- mf hs >>= mutateInTypes p
    o' <- mutateOrder p o
    let n = Vec.map (IntSet.size . _tasks) hs
    return $ C3 o hs'

  encode p (Schedule o t2i i2t) =
    let _insert t is = let i = t2i ! t in is // [(i, insertTask t $ is!i)]
    in C3 o . Vec.filter (not . IntSet.null . _tasks) $
       foldr _insert (Vec.map (flip Host IntSet.empty) i2t) [0..nTask p-1]

  decode p (C3 o hs) = Schedule o (calt2i p hs) (Vec.map _type hs)

calt2i::Problem->Vec.Vector Host->Vec.Vector Int
calt2i p hs = let f i s = map (,i) . IntSet.toList $ _tasks s
              in (Vec.//) (Vec.replicate (nTask p) 0) .
                 concat . Vec.toList $ Vec.imap f hs

mutateByMerging::(RandomGen g)=>Vec.Vector Host->Rand g (Vec.Vector Host)
mutateByMerging hs = do [p0, p1] <- randPos 2 $ Vec.length hs
                        if p0 == p1 then return hs
                          else do h' <- _mergeMutate (hs ! p0, hs ! p1)
                                  return . Vec.ifilter (\i _->i /= p1) $
                                    hs // [(p0, h')]

mutateBySplitting::(RandomGen g)=>Orders->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateBySplitting o hs = do
  p <- getRandomR (0, Vec.length hs - 1)
  [h0, h1] <- _splitMutate o $ hs ! p
  if (IntSet.null $ _tasks h0) ||
     (IntSet.null $ _tasks h1)
    then return hs
    else return . Vec.cons h1 $ hs // [(p, h0)]

mutateInTypes::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateInTypes p hs = do
  let prob = 1 / (fromIntegral $ Vec.length hs) :: Double
      (hs1, hs2) = probSelect prob hs
  hs1' <- Vec.mapM (_typeMutate p) hs1
  return $ hs1' Vec.++ hs2

randomPartition::(RandomGen g)=>Double->Vec.Vector a->Rand g (Vec.Vector a, Vec.Vector a)
randomPartition p vs = do cs <- Vec.replicateM (Vec.length vs) ((<p) <$> getRandom)
                          return $ ( Vec.ifilter (\i _ -> cs!i) vs
                                   , Vec.ifilter (\i _ -> not $ cs!i) vs)
