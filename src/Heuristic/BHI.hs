module Heuristic.BHI (bhil, bhie, bhie2, bhir) where

import           Heuristic   (InfinityPool, PartialSchedule (..), Pool (..))
import           Problem     (Cost, Ins, Problem, Schedule, Time, cu, nTask,
                              nType, qcharge, refTime)

import           Data.List   (minimumBy)
import           Data.Ord    (comparing)
import qualified Data.Vector as Vec
import Control.DeepSeq (NFData (..))

data WeightType = Liner | Exp | Exp2 | Rnd

instance NFData WeightType

instance (NFData pl)=>NFData (CPartial pl) where
  rnf (CPar a b c d e f g h) =
    rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h

data CPartial pl = CPar { _pool        :: pl
                        , _wtype       :: WeightType
                        , _budget      :: Cost
                        , _usedBudget  :: Cost
                        , _remainWork  :: Time
                        , _lastFT      :: Time
                        , _locations   :: Vec.Vector Ins
                        , _finishTimes :: Vec.Vector Time}

instance PartialSchedule CPartial where
  locations = _locations
  finishTimes = _finishTimes
  pool = _pool

  putTask p s t i = s { _pool = pl'
                      , _usedBudget = _usedBudget s + c
                      , _remainWork = _remainWork s - refTime p t
                      , _lastFT = ft
                      , _locations = _locations s Vec.// [(t, i)]
                      , _finishTimes = _finishTimes s Vec.// [(t, ft)]}
    where (_, ft, pl, pl') = allocIns p s t i
          c = cost pl' i - cost pl i

  sortSchedule p ss =
    let ub_min = minimum . map _usedBudget $ ss
        ub_max = maximum . map _usedBudget $ ss
        ft_min = minimum . map _lastFT $ ss
        ft_max = maximum . map _lastFT $ ss
        rw = _remainWork $ head ss
        lr = minimum [qcharge p (ct, 0, rw / cu p ct)|ct<-[0..nType p-1]]
        b = _budget $ head ss
        wt = _wtype $ head ss
        r = if ub_max + lr <= b then 0 else
              if ub_min + lr > b then 1 else
                case wt of
                Liner -> (ub_max + lr - b) / (ub_max - ub_min)
                Exp -> exp $ (ub_min + lr - b) / (ub_max - ub_min)
                Exp2 -> let f x = (exp x - 1)/(exp 1 - 1)
                        in f $ (ub_max + lr - b) / (ub_max - ub_min)
                Rnd -> let x = (ub_min + lr - b) / (ub_max - ub_min)
                       in sqrt $ 1 - x * x
        _worthiness CPar { _usedBudget=ub, _lastFT=ft} =
          let ub_r = (ub - ub_min) / (ub_max - ub_min)
              ft_r = (ft - ft_min) / (ft_max - ft_min)
          in (ub_r * r + ft_r * (1-r), ft_r, ub_r)
    in [minimumBy (comparing _worthiness) ss]

empty::Pool pl=>Problem->Double->WeightType->CPartial pl
empty p b t = CPar (prepare p) t b 0 rw 0
                 (Vec.replicate (nTask p) 0) (Vec.replicate (nTask p) 0)
  where rw = sum . map (refTime p) $ [0..nTask p-1]

bhil::Problem->Double->Schedule
bhil p b = head . schedule p 1 $ (empty p b Liner:: CPartial InfinityPool)

bhie::Problem->Double->Schedule
bhie p b = head . schedule p 1 $ (empty p b Exp:: CPartial InfinityPool)

bhie2::Problem->Double->Schedule
bhie2 p b = head . schedule p 1 $ (empty p b Exp2:: CPartial InfinityPool)

bhir::Problem->Double->Schedule
bhir p b = head . schedule p 1 $ (empty p b Rnd:: CPartial InfinityPool)
