{-# LANGUAGE TupleSections #-}

module Utils.Random (withProb, randPos, uniRandPos, choose, randSplit) where

import           Control.Monad.Random

import           Control.Monad        (replicateM)
import           Data.Functor         ((<$>))
import           Data.List            (sort)
import qualified Data.Vector          as Vec

withProb::(RandomGen g)=>Double->a->a->Rand g a
withProb p a b = do
  r <- getRandomR (0, 1)
  return $ if r <= p then a else b

uniRandPos::(RandomGen g)=>Int->Int->Rand g [Int]
uniRandPos n m = posl [] n
  where posl rs 0 = return $ sort rs
        posl rs k = do p <- getRandomR (0, m-1)
                       if p `elem` rs then posl rs k
                         else posl (p:rs) (k-1)

randPos::(RandomGen g)=>Int->Int->Rand g [Int]
randPos n m = sort <$> (replicateM n $ getRandomR (0, m-1))

choose::(RandomGen g)=>a->a->Rand g a
choose a b = withProb 0.5 a b

randSplit::RandomGen g=>Double->Vec.Vector a->Rand g (Vec.Vector a, Vec.Vector a)
randSplit p vs = do
  (v0, v1) <- Vec.partition snd <$> Vec.mapM fp vs
  return (Vec.map fst v0, Vec.map fst v1)
  where fp x = do f <- withProb 0.5 (,True) (,False)
                  return $ f x
