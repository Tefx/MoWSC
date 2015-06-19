module EA.Chromosome.C0 (C0) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (crossoverOrder, mutateOrder,
                                        onePointCrossover, twoPointCrossover)
import           Problem

import           Control.DeepSeq       (NFData (..))
import           Control.Monad.Random  (getRandomR)
import           Data.Function         (on)
import           Data.List             (groupBy, sortBy)
import qualified Data.Map              as Map
import           Data.Vector           (Vector, (!), (//))
import qualified Data.Vector           as Vec

data C0 = C0 { _order :: Orders
             , _str   :: Vector Int}

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

  encode p (Schedule _o _t2i _i2t) = C0 _o s
    where m = type2ins _i2t $ nTask p
          s = flip Vec.map _t2i $ (Map.!) m

  decode _p i = fromPool (_order i) (_str i)

type2ins::Vector InsType->Int->Map.Map Ins Ins
type2ins ts n = Map.unions .
                map (assIns ts n) .
                groupBy ((==) `on` (ts!)) .
                sortBy (compare `on` (ts!)) $
                [0..Vec.length ts-1]

assIns::Vector InsType->Int->[Ins]->Map.Map Ins Ins
assIns ts n is = Map.fromList $
                 zipWith (\x y->(x, t*n+y)) is [0..]
  where t = ts ! head is
