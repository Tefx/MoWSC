module Utils ( foldM'
             , maxIndex
             , replaceNth
             , unique
             , With (..), attach) where

import           Data.List   (maximumBy, (\\))
import           Data.Ord    (comparing)
import qualified Data.Set    as Set
import qualified Data.Vector as Vec

foldM'::Monad m=>(a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

maxIndex::Ord a=>[a]->Int
maxIndex xs = snd . maximumBy (comparing fst) $ zip xs [0..]

replaceNth::Int->a->[a]->[a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

unique::Ord i=>Vec.Vector i->(Vec.Vector i, Vec.Vector i)
unique pop = (Vec.fromList pop0, Vec.fromList pop1)
  where pop0 = Set.toList . Set.fromList . Vec.toList $ pop
        pop1 = Vec.toList pop \\ pop0

data With a b = With { attached :: a
                     , original :: b}

attach::a->b->With a b
attach = With
