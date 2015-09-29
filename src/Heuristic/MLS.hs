module Heuristic.MLS (mls) where

import           Heuristic       (InfinityPool, PartialSchedule (..), Pool (..))
import           Problem         (Ins, Problem, Schedule, Time, nTask)

import           Control.DeepSeq (NFData (..))
import           Data.List       (minimumBy)
import           Data.Ord        (comparing)
import           Data.Vector     ((//))
import qualified Data.Vector     as Vec

data CPartial pl = CPar { _pool        :: pl
                        , _r           :: [Double]
                        , _ct          :: Double
                        , _cc          :: Double
                        , _finishTimes :: Vec.Vector Time
                        , _locations   :: Vec.Vector Ins}

instance (NFData pl)=>NFData (CPartial pl) where
  rnf (CPar a b c d e f) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f

_worth::Double->Double->Double->Double->Double->CPartial pl->Double
_worth t0 dt c0 dc r (CPar _ _ ct cc _ _) = r * (ct - t0) / dt + (1-r) * (cc - c0) / dc

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  sortSchedule _ ss = let fastest = minimumBy (comparing $ \x->(_ct x, _cc x)) ss
                          cheapest = minimumBy (comparing $ \x->(_cc x, _ct x)) ss
                          t0 = _ct fastest
                          dt = _ct cheapest - t0
                          c0 = _cc cheapest
                          dc = _cc fastest - c0
                      in (fastest:) . (cheapest:) .
                         flip map (_r $ head ss) $
                         \r->minimumBy (comparing $ _worth t0 dt c0 dc r) ss

  putTask p s t i = s { _pool = pl'
                      , _ct = if ft > _ct s then ft else _ct s
                      , _cc = cost pl' i - cost (_pool s) i + _cc s
                      , _finishTimes = _finishTimes s // [(t, ft)]
                      , _locations = _locations s // [(t, i)]}
    where (_, ft, pl') = allocIns p s t i

empty::Pool pl=>Problem->[Double]->CPartial pl
empty p rs = CPar (prepare p) rs 0 0
             (Vec.replicate (nTask p) 0) (Vec.replicate (nTask p) 0)

mls::Problem->Int->[Schedule]
mls p k = let d = 1 / (fromIntegral k - 1)
              rs = map ((d*) . fromIntegral) $ [1..k-2]
          in schedule p k $ (empty p rs :: CPartial InfinityPool)
