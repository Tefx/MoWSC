{-# LANGUAGE BangPatterns #-}

module EA.NSGA2 (WithNSGA2Fit, assignNSGA2Fit, nsga2Select) where

import           EA            (EnvSelector)
import           EA.Fitness    (FitnessAssigner, WithFitness (..), sortByFit)
import           MOP           (Objectives (..))
import           Utils         (With (..))

import           Data.Function (on)
import qualified Data.IntSet   as IntSet
import           Data.List     (foldl', partition, sortBy)
import           Data.Ord      (comparing)
import           Data.Vector   ((!))
import qualified Data.Vector   as Vec


-- Export --

type WithNSGA2Fit = WithFitness NSGA2Fit

assignNSGA2Fit::(Objectives o)=>FitnessAssigner NSGA2Fit c o
assignNSGA2Fit = Vec.fromList . concat . map assignDis . assignFI

nsga2Select::(Objectives o)=>EnvSelector c o
nsga2Select pop0 pop1 = Vec.take (Vec.length pop0) .
                        sortByFit assignNSGA2Fit $ (Vec.++) pop0 pop1

-- Non-export --

data NSGA2Fit = NSGA2Fit { frontIdx :: Int
                         , crowdDs  :: Double} deriving (Eq)

instance Ord NSGA2Fit where
  compare i j = case (compare `on` frontIdx) i j of
                 LT -> LT
                 GT -> GT
                 EQ -> case (compare `on` crowdDs) i j of
                   LT -> GT
                   GT -> LT
                   EQ -> EQ


-- FastDominanteSort related --

assignFI::(Objectives o)=>Vec.Vector (With c o)->[[WithNSGA2Fit c o]]
assignFI ms = zipWith (\x s->map (initNSGA2Fit x) s) [0..] $ _fastNDSort ms
  where initNSGA2Fit i a = WithFit a $ NSGA2Fit i 0

_fastNDSort::(Objectives o)=>Vec.Vector (With c o)->[[With c o]]
_fastNDSort pop = map (map (pop!)) . reverse . fst $ _allFS ds []
  where popl = map elem1 $ Vec.toList pop
        ds = zip [0..] . map (_dominatedSet popl) $ popl

_allFS::[(Int, IntSet.IntSet)]->[[Int]]->([[Int]], [(Int, IntSet.IntSet)])
_allFS [] !rs = (rs, [])
_allFS !ss !rs = let (nr, ss') = _nextF ss
                 in _allFS ss' (nr:rs)

_dominatedSet::(Objectives o)=>[o]->o->IntSet.IntSet
_dominatedSet pop i = IntSet.fromList . map snd . filter ((<<<i) . fst) $ zip pop [0,1..]

_nextF::[(Int, IntSet.IntSet)]->([Int], [(Int, IntSet.IntSet)])
_nextF s = (,) is $ map (_removeINS is) s1
  where (s0, s1) = partition (IntSet.null . snd) s
        is = map fst s0

_removeINS::[Int]->(Int, IntSet.IntSet)->(Int, IntSet.IntSet)
_removeINS [] !s = s
_removeINS !(x:xs) !(s0, s1) = _removeINS xs (s0, IntSet.delete x s1)


-- Crowd Distances related --

assignDis::(Objectives o)=>[WithNSGA2Fit c o]->[WithNSGA2Fit c o]
assignDis is = foldl' _dis1 is [0..nObjs-1]
  where nObjs = dimesion . elem1 . runWithFit . head $ is

_dis1::(Objectives o)=>[WithNSGA2Fit c o]->Int->[WithNSGA2Fit c o]
_dis1 ms i = zipWith _update ms' . _disL . map ((@!i) . elem1 . runWithFit) $ ms'
  where ms' = sortBy (comparing $ (@!i) . elem1 . runWithFit) ms
        _inf = 1e42
        _update (WithFit a (NSGA2Fit i c)) v = WithFit a . NSGA2Fit i $ c+v
        _disL v = zipWith (\x y->(x-y)/(last v - head v))
                  (tail v ++ [_inf]) ((0-_inf):v)
