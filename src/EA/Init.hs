{-# LANGUAGE RankNTypes #-}

module EA.Init ( randType
               , randIns
               , randInsOrType
               , randHEFT
               , randInsOrTypeOrHEFT
               , randTypeAndIns
               , randPool) where

import           EA                   (PopInitialiser)
import           Heuristic.Cheap      (cheap)
import           Heuristic.HEFT       (heft)
import           Problem              (Schedule (..), fromPool, nIns, nTask,
                                       nType)

import           Control.Monad        (liftM2)
import           Control.Monad.Random (RandomGen, getRandomR)
import           Data.Functor         ((<$>))
import qualified Data.Vector          as Vec

randType::PopInitialiser
randType p n = Vec.replicateM n $ do
  let t2i = Vec.replicate (nTask p) 0
  i2t <- Vec.singleton <$> getRandomR (0, nType p - 1)
  return $ Schedule [0..nTask p-1] t2i i2t

randIns::PopInitialiser
randIns p n = Vec.replicateM n $ do
  t2i <- Vec.replicateM (nTask p) $ getRandomR (0, nIns p - 1)
  i2t <- Vec.replicate (nIns p) <$> getRandomR (0, nType p - 1)
  return $ Schedule [0..nTask p-1] t2i i2t

randTypeAndIns::PopInitialiser
randTypeAndIns p n = Vec.replicateM n $ do
  t2i <- Vec.replicateM (nTask p) $ getRandomR (0, nIns p - 1)
  i2t <- Vec.replicateM (nIns p) $ getRandomR (0, nType p - 1)
  return $ Schedule [0..nTask p-1] t2i i2t

randHEFT::PopInitialiser
randHEFT p n = let f = heft p
                   c = cheap p
               in return . Vec.fromList . concat $ replicate (div n 2) [f, c]

randInsOrType::PopInitialiser
randInsOrType p n = (liftM2 (Vec.++)) (randIns p m) (randType p m)
  where m = div n 2

randInsOrTypeOrHEFT::PopInitialiser
randInsOrTypeOrHEFT p n = (liftM2 (Vec.++)) (randInsOrType p $ n-2) (randHEFT p 2)

randInsInType::PopInitialiser
randInsInType p n = Vec.replicateM n $ do
  num_ins <- getRandomR (1, nTask p)
  t2i <- Vec.replicateM (nTask p) $ getRandomR (0, num_ins - 1)
  i2t <- Vec.replicate num_ins <$> getRandomR (0, nType p - 1)
  return $ Schedule [0..nTask p-1] t2i i2t

randPool::PopInitialiser
randPool p n = Vec.replicateM n $ do
  loc <- Vec.replicateM (nTask p) $ getRandomR (0, nTask p * nType p - 1)
  return $ fromPool [0..nTask p-1] loc
