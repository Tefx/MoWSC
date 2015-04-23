module EA.Chromosome.C1 (C1) where

import           EA                    (Chromosome (..))
import           EA.Chromosome.Generic (crossoverOrder, mutateOrder)
import           Problem

import           Control.Monad.Random  (Rand, RandomGen, getRandomR)
import           Data.Functor          ((<$>))
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as Vec

data C1 = C1 { _order    :: [Task]
             , _task2ins :: Vec.Vector Ins
             , _ins2type :: Vec.Vector InsType}

instance Chromosome C1 where
  repMode _ = (2, 2)

  crossover p [t0,t1] = do
    [o0, o1] <- crossoverOrder p (_order t0) (_order t1)
    [(t2i0, i2t0), (t2i1, i2t1)] <- crossoverIns p (_task2ins t0)
                                                   (_task2ins t1)
                                                   (_ins2type t0)
                                                   (_ins2type t1)
    return [C1 o0 t2i0 i2t0, C1 o1 t2i1 i2t1]

  mutate p i = do
    order' <- mutateOrder p $ _order i
    task2ins' <- fastMutate (nTask p - 1) (nIns p - 1) $ _task2ins i
    ins2type' <- fastMutate (nIns p - 1) (nType p - 1) $ _ins2type i
    return $ C1 order' task2ins' ins2type'

  encode p (Schedule o t2i i2t) = C1 o t2i . (Vec.++) i2t $
                                  Vec.replicate (nIns p - Vec.length i2t) 0

  decode _ (C1 _o _t2i _i2t) =
    let is = Set.toAscList . Set.fromList $ Vec.toList _t2i
        i2t' = Vec.fromList . map (_i2t!) $ is
        m = Map.fromList $ zip is [0..]
        t2i' = Vec.map ((Map.!) m) _t2i
    in Schedule _o t2i' i2t'

fastMutate::RandomGen g=>Int->Int->Vec.Vector Int->Rand g (Vec.Vector Int)
fastMutate r0 r1 s = do pos <- getRandomR (0, r0)
                        v <- getRandomR (0, r1)
                        return $ s // [(pos, v)]

randomChoice::RandomGen g=>a->a->Rand g a
randomChoice x y = f x y <$> getRandomR (0, 1)
  where f a b p = if p == 0 then a else b
        f::a->a->Int->a

assignVec::Vec.Vector a->Int->a->Vec.Vector a
assignVec v i a = v // [(i, a)]

assignType::RandomGen g=>Problem->
            [Ins]->
            (Set.Set Ins)->
            (Vec.Vector InsType)->
            (Vec.Vector InsType)->
            Rand g (Vec.Vector InsType)
assignType _ [] _ _ res = return res
assignType p (i:is) s i2t0 res = res' >>= assignType p is s i2t0
  where typ = i2t0 ! i
        typ' = if Set.member i s
               then randomChoice typ (res ! i)
               else return typ
        res' = assignVec res i <$> typ'

crossoverIns::RandomGen g=>Problem->
              Vec.Vector Ins -> Vec.Vector Ins->
              Vec.Vector InsType -> Vec.Vector InsType ->
              Rand g [(Vec.Vector Ins, Vec.Vector InsType)]
crossoverIns p t2ins0 t2ins1 i2type0 i2type1 = do
  l <- getRandomR (0, nTask p - 1)
  i2type0' <- assignType p (Vec.toList $ Vec.take l t2ins0)
                          (Set.fromList . Vec.toList $ Vec.drop l t2ins1)
                          i2type0
                          i2type1
  i2type1' <- assignType p (Vec.toList $ Vec.take l t2ins1)
                          (Set.fromList . Vec.toList $ Vec.drop l t2ins0)
                          i2type1
                          i2type0
  let r0 = ((Vec.++) (Vec.take l t2ins0) (Vec.drop l t2ins1), i2type0')
  let r1 = ((Vec.++) (Vec.take l t2ins1) (Vec.drop l t2ins0), i2type1')
  return [r0, r1]
