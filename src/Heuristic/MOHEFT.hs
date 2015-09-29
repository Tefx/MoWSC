{-# LANGUAGE TypeFamilies #-}

module Heuristic.MOHEFT (moheft) where

import           EA              (envSelectN)
import           EA.Foreign      as EF
import           EA.NSGA2        as EA_NSGA2
import           Heuristic       (FullPool, InfinityPool, PartialSchedule (..),
                                  Pool (..))
import           MOP             (MakespanCost, Objectives (..), WithObjs (..))
import           Problem

import           Control.DeepSeq (NFData (..))
import           Data.Vector     (Vector, (//))
import qualified Data.Vector     as Vec

data CPartial pl = CPar { _pool        :: pl
                        , _k           :: Int
                        , _ct          :: Double
                        , _cc          :: Double
                        , _finishTimes :: Vector Time
                        , _locations   :: Vector Ins}

instance (NFData pl)=>NFData (CPartial pl) where
  rnf (CPar a b c d e f) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

instance WithObjs (CPartial pl) where
  type Objs (CPartial pl) = MakespanCost
  getObjs p = fromList $ [_ct p, _cc p]

instance Show (CPartial pl) where
  show = show . toList. getObjs

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  putTask p s t i = let (_, ft, pl') = allocIns p s t i
                    in s { _pool = pl'
                         , _ct = if ft > _ct s then ft else _ct s
                         , _cc = cost pl' i - cost (_pool s) i + _cc s
                         , _finishTimes = _finishTimes s // [(t, ft)]
                         , _locations = _locations s // [(t, i)]}

  sortSchedule _ ss = if length ss > k
                      then Vec.toList . envSelectN EF.nsga2Select k $ Vec.fromList ss
                      else ss
    where k = _k $ head ss


empty::Pool pl=>Problem->Int->CPartial pl
empty p k = CPar (prepare p) k 0 0
            (Vec.replicate (nTask p) 0) (Vec.replicate (nTask p) 0)

moheft::Problem->Int->[Schedule]
moheft p k = schedule p k $ (empty p k::CPartial InfinityPool)
--moheft p k = schedule p k $ (empty p k::CPartial FullPool)
