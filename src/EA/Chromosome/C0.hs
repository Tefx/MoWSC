module EA.Chromosome.C0 (C0) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (crossoverOrder, mutateOrder,
                                        onePointCrossover, twoPointCrossover)
import           Problem

import           Control.DeepSeq       (NFData (..))
import           Control.Monad.Random  (getRandomR)
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

data C0 = C0 { _order :: Orders
             , _str   :: Vec.Vector Int}

instance NFData C0 where
  rnf (C0 _o _s) = rnf _o `seq` rnf _s

instance Chromosome C0 where
  repMode _ = (2, 2)

  mutate p _ i = do
    o' <- mutateOrder p $ _order i
    pos <- getRandomR (0, nTask p-1)
    l' <- getRandomR (0, nTask p * nType p - 1)
    return . C0 o' $ _str i // [(pos, l')]

  crossover p _ [i0, i1] = do
    [s0, s1] <- twoPointCrossover (_str i0) (_str i1)
    [o0, o1] <- crossoverOrder p (_order i0) (_order i1)
    return $ [C0 o0 s0, C0 o1 s1]

  encode p s = return $ C0 o str
    where (o, str) = toPool p s

  decode _ i = fromPool (_order i) (_str i)
