{-# LANGUAGE TupleSections #-}

module EA.Chromosome.C5 (C5) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Foreign (olTrans, probSelect)
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (Ins, InsType, Orders, Problem,
                                        Schedule (Schedule), Task, nTask, nType)
import           Utils.Random          (choose, doWithProb, probApply, randPos,
                                        rouletteSelect)

import           Control.DeepSeq       (NFData (..), force)
import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandom, getRandomR)
import           Data.Foldable         (foldrM)
import           Data.Function         (on)
import           Data.Functor          ((<$>))
import qualified Data.IntSet           as IntSet
import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

import           Debug.Trace           (traceShow)

data Host = Host { _tasks :: [Task]
                 , _type  :: InsType}

instance NFData Host where
  rnf (Host _t _p) = rnf _t `seq` rnf _p `seq` ()

mergeList::Vec.Vector Int->[Task]->[Task]->[Task]
mergeList _ [] ys = ys
mergeList _ xs [] = xs
mergeList loc (x:xs) (y:ys)
  | loc ! x < loc ! y = x:mergeList loc xs (y:ys)
  | otherwise = y:mergeList loc (x:xs) ys

mergeHosts::(RandomGen g)=>Vec.Vector Int->Host->Host->Rand g Host
mergeHosts loc (Host ts0 t0) (Host ts1 t1) =
  Host (mergeList loc ts0 ts1) <$> choose t0 t1

splitList::[a]->[Int]->[[a]]
splitList xs [l0,l1] = let xs0 = drop l0 $ take l1 xs
                           xs1 = take l0 xs ++ drop l1 xs
                       in [xs0, xs1]

splitHost::(RandomGen g)=>Host->Rand g [Host]
splitHost (Host ts t) =
  map (flip Host t) . splitList ts <$> (randPos 2 . (+1) $ length ts)

changeType::(RandomGen g)=>Problem->Host->Rand g Host
changeType p (Host ts t) = Host ts <$> getRandomR (0, nType p-1)

data C5 = C5 { _locs :: Vec.Vector Int
             , _inss :: Vec.Vector Host}

instance NFData C5 where
  rnf (C5 _l _i) = rnf _l `seq` rnf _i `seq` ()

instance Chromosome C5 where
  repMode _ = (1, 1)

  mutate p _ (C5 locs hs) = do
    let prob = 1 / (fromIntegral $ Vec.length hs)
    mf <- choose (mutateByMerging prob locs) $ (mutateBySplitting prob)
    hs' <- mf hs >>= mutateInTypes prob p
    locs' <- orders2Locs <$> (mutateOrder p $ locs2Orders locs)
    return $ C5 locs hs'

  encode p (Schedule o t2i i2t) =
    let hs = Vec.map (Host []) i2t
        hs' = foldr (insertTask t2i) hs o
    in return $ C5 (orders2Locs o) hs'

  decode p (C5 locs hs) =
    Schedule (locs2Orders locs) (calt2i p hs) (Vec.map _type hs)

locs2Orders::Vec.Vector Int->Orders
locs2Orders = olTrans

orders2Locs::Orders->Vec.Vector Int
orders2Locs = Vec.fromList . olTrans . Vec.fromList

insertTask::Vec.Vector Ins->Task->Vec.Vector Host->Vec.Vector Host
insertTask t2i t hs = let loc = t2i ! t
                          (Host ts p) = hs ! loc
                      in hs // [(loc, Host (t:ts) p)]

calt2i::Problem->Vec.Vector Host->Vec.Vector Int
calt2i p hs = (Vec.//) (Vec.replicate (nTask p) 0) .
              concat . Vec.toList $ Vec.imap (\i h->map (,i) $ _tasks h) hs

mergeToHosts::(RandomGen g)=>Vec.Vector Int->Vec.Vector Host->Host->Rand g (Vec.Vector Host)
mergeToHosts locs hs h= do
  l <- getRandomR (0, Vec.length hs-1)
  h' <- mergeHosts locs h $ hs!l
  return $ hs // [(l, h')]

mutateByMerging::(RandomGen g)=>
                 Double->Vec.Vector Int->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateByMerging prob locs hs = do
  let (hs0, hs1) = probSelect prob hs
  if Vec.null hs0 || Vec.null hs1 then return hs
    else Vec.foldM (mergeToHosts locs) hs1 hs0

mutateBySplitting::(RandomGen g)=>Double->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateBySplitting prob hs = do
  let (hs0, hs1) = probSelect prob hs
  hss <- Vec.mapM splitHost hs0
  return . (Vec.++) hs1 . Vec.fromList . concat $ Vec.toList hss

mutateInTypes::(RandomGen g)=>Double->Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateInTypes prob p hs = do
  let (hs0, hs1) = probSelect prob hs
  hs0' <- Vec.mapM (changeType p) hs0
  return $ hs0' Vec.++ hs1
