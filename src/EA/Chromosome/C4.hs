{-# LANGUAGE TupleSections #-}

module EA.Chromosome.C4 (C4) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (mutateOrder)
import           Problem               (InsType, Orders, Problem, Schedule (..),
                                        Task, nTask, nType)
import           Utils.Random          (choose, doWithProb, randPos)

import           Control.Monad         (join)
import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import           Data.Functor          ((<$>))
import           Data.List             (partition)
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

data Host = Host { _type  :: InsType
                 , _tasks :: [Task]}

absorb::(RandomGen g)=>Problem->Host->Host->Rand g Host
absorb _ (Host t0 tl0) (Host t1 tl1) = flip Host (tl0 ++ tl1) <$> choose t0 t1

reject::(RandomGen g)=>Problem->Host->Rand g [Host]
reject p (Host t tl) =
  let pb = (1/) . fromIntegral $ length tl
  in do (tl0, tl1) <- partition snd <$>
                      mapM (doWithProb pb (,True) (,False)) tl
        t0 <- getRandomR (0, nType p-1)
        return [Host t0 $ map fst tl0, Host t $ map fst tl1]

scale::(RandomGen g)=>Problem->Host->Rand g Host
scale p (Host t tl) = flip Host tl <$> getRandomR (0, nType p-1)

insertTask::Task->Host->Host
insertTask x (Host t tl) = Host t (x:tl)

filterUsed::Vec.Vector Host->Vec.Vector Host
filterUsed = Vec.filter (not . null . _tasks)

data C4 = C4 { _order :: Orders
             , _hosts :: Vec.Vector Host}

instance Chromosome C4 where
  repMode _ = (1, 1)

  encode p (Schedule o t2i i2t) =
    let _updT t hs = let i = t2i!t in hs // [(i, insertTask t $ hs!i)]
    in C4 o . filterUsed $
       foldr _updT (Vec.map (flip Host []) i2t) [0..nTask p-1]

  decode p (C4 o hs) =
    let i2t = Vec.map _type hs
        t2i = Vec.foldr (flip (//)) (Vec.replicate (nTask p) 0) $
              Vec.imap (\i->map (,i) . _tasks) hs
    in Schedule o t2i i2t

  mutate p _ (C4 o hs) = do
    hs' <- mutateByRejection p hs >>=
           mutateByAbsorption p >>=
           mutateInTypes p
    o' <- mutateOrder p o
    return $ C4 o' hs'

mutateProb::Double->Vec.Vector Host->Double
mutateProb i hs = (i/) . fromIntegral $ Vec.length hs

mutateInTypes::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateInTypes p hs =
  flip Vec.mapM hs $ join . doWithProb (mutateProb 1 hs) (scale p) return

mutateByRejection::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateByRejection p hs =
  filterUsed . Vec.fromList . concat . Vec.toList <$>
  Vec.mapM (join . doWithProb (mutateProb 1 hs) (reject p) (return . (:[]))) hs

mutateByAbsorption::(RandomGen g)=>Problem->Vec.Vector Host->Rand g (Vec.Vector Host)
mutateByAbsorption p hs =
  do [p0, p1] <- randPos 2 $ Vec.length hs
     if p0 == p1 then return hs
       else do h' <- absorb p (hs!p0) (hs!p1)
               return . Vec.ifilter (\i _->i /= p1) $ hs // [(p0, h')]
