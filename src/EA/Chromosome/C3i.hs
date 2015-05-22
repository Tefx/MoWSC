{-# LANGUAGE TupleSections #-}

module EA.Chromosome.C3i (C3i) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (..), Task, nIns, nTask, nType)
import           Utils.Random          (choose, doWithProb, randPos)

import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import           Data.Functor          ((<$>))
import           Data.List             (partition)
import           Data.Maybe            (fromJust)
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

data Host = Host { _types :: InsType
                 , _hosts :: [Task]}

mergeHosts::(RandomGen g)=>Host->Host->Rand g Host
mergeHosts (Host t0 ts0) (Host t1 ts1) = do
  t <- choose t0 t1
  return . Host t $ ts0 ++ ts1

splitHost::(RandomGen g)=>Problem->Host->Rand g (Host, Host)
splitHost p (Host t ts) = do
  [t0, t1] <- randPos 2 $ nType p
  let (ts0, ts1) = partition (\x->x>t0 && x<t1) ts
  return (Host t ts0, Host t ts1)

scaleHost::(RandomGen g)=>Problem->Host->Rand g Host
scaleHost p (Host t ts) = flip Host ts <$>  getRandomR (0, nType p-1)

nullHost::Host->Bool
nullHost = null . _hosts

addTask::Task->Host->Host
addTask t (Host typ ts) = Host typ $ t:ts

data C3i = C3i Orders (Vec.Vector Host)

instance Chromosome C3i where
  repMode _ = (1, 1)

  mutate p _ (C3i o hs) = do
    mf <- choose mutateByMerging mutateBySplitting
    hs' <- mf p hs >>= join . doWithProb 0.9 (mutateInTypes p) return
    o' <- mutateOrder p o
    return $ C3i o' hs'

  encode p (Schedule o t2i i2t) =
    let insert t hs = let i = t2i!t in hs // [(i, addTask t $ hs!i)]
    in C3i o . Vec.filter (not . nullHost) $
       foldr insert (Vec.map (flip Host []) i2t) [0..nTask p-1]

  decode p (C3i o hs) =
    let i2t = Vec.map _types hs
        assign i (Host t ts) = map (,i) ts
        t2i = Vec.foldr (flip (//)) (Vec.replicate (nTask p) 0) $
              Vec.imap assign hs
    in Schedule o t2i i2t

mutateByMerging::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateByMerging _ hs = do
  [p0, p1] <- randPos 2 $ Vec.length hs
  if p0 == p1 then return hs
    else do h' <- mergeHosts (hs!p0) (hs!p1)
            return . Vec.ifilter (\i _ -> i /= p1) $ hs // [(p0, h')]

mutateBySplitting::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateBySplitting p hs = do
  pos <- getRandomR (0, Vec.length hs - 1)
  (h0, h1) <- splitHost p $ hs!pos
  if nullHost h0 || nullHost h1 then return hs
    else return . Vec.cons h1 $ hs // [(pos, h0)]

mutateInTypes::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateInTypes p hs =
  flip Vec.mapM hs $
  join . doWithProb (1.0 / (fromIntegral $ Vec.length hs)) (scaleHost p) return
