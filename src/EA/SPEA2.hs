{-# LANGUAGE TupleSections #-}

module EA.SPEA2 (WithSPEA2Fit, assignSPEA2Fit, spea2Select) where

import           EA            (EnvSelector)
import           EA.Utils      (FitnessAssigner, WithFitness (..), fitness)
import           MOP           (Normaliser, Objectives, WithObjs (..), initNorm,
                                norm, (<<<))
import           Utils         (With, attach, original)
import           Utils.Math    (euclideanDis)

import           Data.Function (on)
import           Data.List     (sort, sortBy)
import qualified Data.Vector   as Vec

type WithSPEA2Fit o = WithFitness SPEA2Fit o

assignSPEA2Fit::FitnessAssigner SPEA2Fit
assignSPEA2Fit is = let nz = initNorm is
                        rs = Vec.map (fromIntegral) . calRaw . calS $ is
                        k = round . sqrt . fromIntegral $ Vec.length is
                        ds = Vec.map ((1/) . (+2) . (!!k)) $ calDs nz is
                    in Vec.zipWith attach is $
                       Vec.zipWith (+) rs ds

spea2Select::EnvSelector
spea2Select pop0 pop1 = let pop = (Vec.++) pop0 pop1
                            n = Vec.length pop0
                            popD = assignSPEA2Fit pop
                            (p_, p) = Vec.unstablePartition ((<1) . fitness) popD
                        in case compare (Vec.length p_) n of
                            EQ -> Vec.map original p_
                            LT -> Vec.map original .
                                  (Vec.++) p_ .
                                  Vec.fromList. take (n - Vec.length p_) .
                                  sortBy (compare `on` fitness) . Vec.toList $ p
                            GT -> trunc (initNorm pop) n $ Vec.map original p_


-- Non-export --

type SPEA2Fit = Double


-- assign Fitness --

calS::(WithObjs o)=>Vec.Vector o->Vec.Vector (o, Int)
calS pop = Vec.map (\x->(x,) . Vec.length $
                        Vec.filter ((<<<) (getObjs x) . getObjs) pop) pop

calRaw::(WithObjs o)=>Vec.Vector (o, Int)->Vec.Vector Int
calRaw is = Vec.map (\(x, _)->Vec.sum . Vec.map snd $
                              Vec.filter ((<<< (getObjs x)) . getObjs . fst) is) is

eucInd::(WithObjs o)=>Normaliser->o->o->Double
eucInd nz = euclideanDis `on` (norm nz)

calDs::(WithObjs o)=>Normaliser->Vec.Vector o->Vec.Vector [Double]
calDs nz is = Vec.map (\i->sort . Vec.toList $ Vec.map ((eucInd nz) i) is) is

-- select --

compareByKD::(a, [Double])->(a, [Double])->Ordering
compareByKD (_, []) (_, []) = EQ
compareByKD (a, x:xs) (b, y:ys)
  | x == y = compareByKD (a, xs) (b, ys)
  | otherwise = compare x y

trunc::(WithObjs o)=>Normaliser->Int->Vec.Vector o->Vec.Vector o
trunc nz n pop
  | n == Vec.length pop = pop
  | otherwise = let pd = Vec.zipWith (,) pop $ calDs nz pop
                    r = Vec.minIndexBy compareByKD pd
                in trunc nz n $ Vec.ifilter (\i _->i /= r) pop

--trunc'::(WithObjs o)=>Normaliser->Int->Vec.Vector [Double]->Vec.Vectir o->Vec.Vector o
