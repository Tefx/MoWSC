{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module MOP ( Objectives (..), WithObjs (..)
           , initNorm, norm, Normaliser
           , MakespanCost
           , ObjValue) where

import           Control.DeepSeq (NFData (..))
import qualified Data.Vector     as Vec
import           Numeric         (showFFloat)

type ObjValue = Double

class (Show a, NFData a)=>Objectives a where
  dimesion :: a->Int
  (@!)     :: a->Int->ObjValue
  (<<<)    :: a->a->Bool
  toList   :: a->[ObjValue]
  fromList :: [ObjValue]->a

  toList a = map (a@!) [0..dimesion a-1]

class (Show a, Objectives (Objs a))=>WithObjs a where
  type Objs a:: *
  getObjs::a->Objs a

data Normaliser = Norm { _dim :: Int
                       , _ms  :: [Double]
                       , _ds  :: [Double]}

initNorm::(WithObjs o)=>Vec.Vector o->Normaliser
initNorm is = Norm n mins $ zipWith (-) maxs mins
  where n = dimesion . getObjs $ Vec.head is
        maxs = flip map [0..n-1] $ \x->Vec.maximum $ Vec.map ((@!x) . getObjs) is
        mins = flip map [0..n-1] $ \x->Vec.minimum $ Vec.map ((@!x) . getObjs) is

norm::(WithObjs o)=>Normaliser->o->[Double]
norm nz i = let _norm x y z = (x-y)/z
            in zipWith3 _norm (toList $ getObjs i) (_ms nz) (_ds nz)

data MakespanCost = MC { makespan :: Double
                       , cost     :: Double}

instance NFData MakespanCost where
  rnf (MC m c) = rnf m `seq` rnf c

instance Show MakespanCost where
  show a = showFFloat (Just 2) (makespan a) " " ++ showFFloat (Just 2) (cost a) ""

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
