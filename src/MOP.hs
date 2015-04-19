module MOP ( Objectives (..)
           , initNorm, norm, Normaliser
           , MakespanCost) where

import qualified Data.Vector as Vec

import           Utils

type ObjValue = Double

class Objectives a where
  dimesion::a->Int
  (@!)::a->Int->ObjValue
  (<<<)::a->a->Bool
  toList::a->[ObjValue]
  fromList::[ObjValue]->a

  dimesion = length . toList
  toList a = map (a@!) [0..dimesion a-1]

data Normaliser = Norm { _ms :: [Double]
                       , _ds :: [Double]}

initNorm::(Objectives o)=>Vec.Vector (With c o)->Normaliser
initNorm is = Norm mins $ zipWith (-) maxs mins
  where n = dimesion . elem1 $ Vec.head is
        maxs = flip map [0..n-1] $ \x->Vec.maximum $ Vec.map ((@!x) . elem1) is
        mins = flip map [0..n-1] $ \x->Vec.minimum $ Vec.map ((@!x) . elem1) is

norm::(Objectives o)=>Normaliser->With c o->[Double]
norm nz i = zipWith3 (\x y z->(x-y)/z) (toList $ elem1 i) (_ms nz) (_ds nz)


data MakespanCost = MC { makespan :: Double
                       , cost     :: Double}

instance Objectives MakespanCost where
  dimesion _ = 2

  a @! 0 = makespan a
  a @! 1 = cost a

  MC x0 y0 <<< MC x1 y1
    | x0 < x1 = y0 <= y1
    | x0 == x1 = y0 < y1
    | x0 > x1 = False

  fromList [x, y] = MC x y
  toList (MC x y) = [x, y]
