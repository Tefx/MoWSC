module Utils.Random (withProb, getRandPos, choose) where

import           Control.Monad.Random

withProb::(RandomGen g)=>Double->a->a->Rand g a
withProb p a b = do
  r <- getRandomR (0, 1)
  return $ if r <= p then a else b

getRandPos::(RandomGen g)=>Int->Int->Rand g [Int]
getRandPos n m = posl [] n
  where posl rs 0 = return rs
        posl rs k = do p <- getRandomR (0, m-1)
                       if p `elem` rs then posl rs k
                         else posl (p:rs) (k-1)

choose::(RandomGen g)=>a->a->Rand g a
choose a b = withProb 0.5 a b
