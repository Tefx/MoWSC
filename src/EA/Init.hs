{-# LANGUAGE RankNTypes #-}

module EA.Init ( randTypeSingle, randTypeAll, randTypeR, randTypeP
               , randIns
               , randInsOrType
               , randHEFT
               , randInsOrTypeOrHEFT
               , randInsInType
               , randPool) where

import           EA                   (PopInitialiser)
import           Heuristic            (getOrder)
import           Heuristic.Cheap      (cheap)
import           Heuristic.HEFT       (heft)
import           Problem              (Problem, Schedule (..), cu, fromPool,
                                       insPrice, nIns, nTask, nType)

import           Control.Monad        (liftM2)
import           Control.Monad.Random (RandomGen, getRandomR)
import           Data.Functor         ((<$>))
import           Data.List            (maximumBy)
import           Data.Ord             (comparing)
import qualified Data.Vector          as Vec

randTypeSingle::PopInitialiser
randTypeSingle p n = Vec.replicateM n $ do
  let t2i = Vec.replicate (nTask p) 0
  i2t <- Vec.singleton <$> getRandomR (0, nType p - 1)
  return $ Schedule (getOrder p) t2i i2t

randTypeAll::PopInitialiser
randTypeAll p n = Vec.replicateM n $ do
  let t2i = Vec.fromList [0..nTask p-1]
  i2t <- Vec.replicate (nTask p) <$> getRandomR (0, nType p - 1)
  return $ Schedule (getOrder p) t2i i2t

randTypeR::PopInitialiser
randTypeR p n = Vec.replicateM n $ do
  n <- getRandomR (1, nTask p)
  t2i <- Vec.replicateM (nTask p) $ getRandomR (0, n-1)
  i2t <- Vec.replicate n <$> getRandomR (0, nType p - 1)
  return $ Schedule (getOrder p) t2i i2t

randTypeP::PopInitialiser
randTypeP p n = do let k = div n 5
                   is0 <- randTypeAll p k
                   is1 <- randTypeSingle p k
                   is2 <- randTypeR p $ k * 3
                   (Vec.++) is0 . (Vec.++) is1 . (Vec.++) is2 <$>
                     randHEFT p 2

randIns::PopInitialiser
randIns p n = Vec.replicateM n $ do
  t2i <- Vec.replicateM (nTask p) $ getRandomR (0, nIns p - 1)
  i2t <- Vec.replicate (nTask p) <$> getRandomR (0, nType p - 1)
  return $ Schedule (getOrder p) t2i i2t

randHEFT::PopInitialiser
randHEFT p n = let f = heft p
                   c = cheap p
               in return . Vec.fromList . concat $ replicate (div n 2) [f, c]

fastest::Problem->Schedule
fastest p = let h = maximumBy (comparing $ cu p) [0..nType p - 1]
                t2i = Vec.fromList $ [0..nTask p - 1]
                i2t = Vec.replicate (nTask p) h
            in Schedule (getOrder p) t2i i2t

cheapest::Problem->Schedule
cheapest p = let h = maximumBy (comparing $ \x -> cu p x / insPrice p x) [0..nType p - 1]
                 t2i = Vec.replicate (nTask p) 0
                 i2t = Vec.singleton h
             in Schedule (getOrder p) t2i i2t

extreme::Problem->Vec.Vector Schedule
extreme p = Vec.fromList [fastest p, cheapest p]

randInsOrType::PopInitialiser
randInsOrType p n = (liftM2 (Vec.++)) (randTypeR p m) (randTypeSingle p m)
  where m = div n 2

randInsOrTypeOrHEFT::PopInitialiser
randInsOrTypeOrHEFT p n = (liftM2 (Vec.++)) (randHEFT p 2) (randInsOrType p $ n - 2)

randInsInType::PopInitialiser
randInsInType p n = Vec.replicateM n $ do
  num_ins <- getRandomR (1, nTask p)
  t2i <- Vec.replicateM (nTask p) $ getRandomR (0, num_ins - 1)
  i2t <- Vec.replicate num_ins <$> getRandomR (0, nType p - 1)
  return $ Schedule (getOrder p) t2i i2t

randPool::PopInitialiser
randPool p n = Vec.replicateM n $ do
  loc <- Vec.replicateM (nTask p) $ getRandomR (0, nTask p * nType p - 1)
  return $ fromPool [0..nTask p-1] loc
