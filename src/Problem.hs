{-# LANGUAGE ExistentialQuantification #-}

module Problem ( Time, Data
               , Ins, InsType, CU, Bandwidth, Cost, Account (..)
               , Task, Orders
               , Workflow (..), Service (..)
               , Problem (..), preds, succs, inp, ins, comm, refTime, nTask,
                 cu, bw, nIns, nType, charge, insPrice, qcharge, insPricePerCU
               , calObjs, showObjs
               , Schedule (..), fromPool, type2ins, toPool) where

import           Control.Monad       (forM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Function       (on)
import           Data.Functor        ((<$>))
import           Data.List           (groupBy, sortBy)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import           Numeric             (showFFloat)

type Time = Double
type Data = Double


-- Workflow related --

type Task = Int
type Orders = [Task]

class Workflow w where
  w_preds::w->Task->[Task]
  w_preds w t = filter (\x->w_hasEdge w x t) [0..w_nTask w-1]

  w_succs::w->Task->[Task]
  w_succs w t = filter (\x->w_hasEdge w t x) [0..w_nTask w-1]

  w_hasEdge::w->Task->Task->Bool
  w_hasEdge w ti tj = ti `elem` w_preds w tj

  w_comm::w->Task->Task->Data
  w_refTime::w->Task->Time

  w_nTask::w->Int


-- Service related --

type Ins = Int
type InsType = Int
type CU = Double
type Bandwidth = Double
type Cost = Double
newtype Account = Account { insUsage :: [(InsType, Time, Time)] }

class Service s where

  s_cu::s->InsType->CU

  s_bw::s->InsType->InsType->Bandwidth

  s_maxIns::s->Int
  s_nType::s->Int

  s_typeLimit::s->InsType->Int
  s_typeLimit s _ = s_maxIns s

  s_charge::s->Account->Cost
  s_charge s = sum . map (s_qcharge s) . insUsage

  s_insPrice::s->InsType->Cost

  s_qcharge::s->(InsType, Time, Time)->Cost

-- Problem related --

data Problem = forall w s. (Workflow w, Service s)=>Prob w s

preds::Problem->Task->[Task]
preds (Prob w _) = w_preds w

succs::Problem->Task->[Task]
succs (Prob w _) = w_succs w

inp::Problem->Task->Task->Bool
inp (Prob w _) ti tj = w_hasEdge w ti tj

ins::Problem->Task->Task->Bool
ins (Prob w _) tj ti = w_hasEdge w ti tj

comm::Problem->Task->Task->Data
comm (Prob w _) = w_comm w

refTime::Problem->Task->Time
refTime (Prob w _) = w_refTime w

nTask::Problem->Int
nTask (Prob w _) = w_nTask w

cu::Problem->InsType->CU
cu (Prob _ s) = s_cu s

bw::Problem->InsType->InsType->Bandwidth
bw (Prob _ s) = s_bw s

nIns::Problem->Int
nIns (Prob w s) = min (s_maxIns s) (w_nTask w)

nType::Problem->Int
nType (Prob _ s) = s_nType s

charge::Problem->Account->Cost
charge (Prob _ s)= s_charge s

qcharge::Problem->(InsType, Time, Time)->Cost
qcharge (Prob _ s) = s_qcharge s

insPrice::Problem->InsType->Cost
insPrice (Prob _ s) = s_insPrice s

insPricePerCU::Problem->InsType->Cost
insPricePerCU p t = insPrice p t / cu p t


data Schedule = Schedule { orderStr    :: [Task]
                         , task2insStr :: Vector Ins
                         , ins2typeStr :: Vector InsType} deriving (Show)

task2ins::Schedule->Task->Ins
task2ins a t = task2insStr a ! t

ins2type::Schedule->Ins->InsType
ins2type a t = ins2typeStr a ! t

task2type::Schedule->Task->InsType
task2type a t = ins2type a . task2ins a $ t

calObjs::Problem->Schedule->[Double]
calObjs p s = runST $ simulate p s

showObjs::Problem->Schedule->String
showObjs p s = showFFloat (Just 2) x " " ++ showFFloat (Just 2) y ""
  where [x, y] = calObjs p s

comptime::Problem->Schedule->Task->Time
comptime p s task = refTime p task / cu p (task2type s task)

-- Non-exports Schedule-problem-specific --

data SimState s = SS { _aft  :: MVec.STVector s Time
                     , _inss :: MVec.STVector s Time
                     , _insf :: MVec.STVector s Time}

astTask::Problem->Schedule->SimState st->Task->ST st Time
astTask p s (SS _aft _inss _insf) task =
  do lst <- MVec.unsafeRead _insf . flip task2ins task $ s
     foldr max lst <$> (mapM (MVec.unsafeRead _aft) $ preds p task)

scheduleTask::Problem->Schedule->SimState st->Task->ST st ()
scheduleTask p s ss task = do st <- astTask p s ss task
                              let l = task2ins s task
                                  ft = st + comptime p s task
                              MVec.unsafeWrite (_aft ss) task ft
                              stl <- MVec.unsafeRead (_inss ss) l
                              if stl < 0
                                then MVec.unsafeWrite (_inss ss) l st
                                else return ()
                              MVec.unsafeWrite (_insf ss) l ft

simulate::Problem->Schedule->ST st [Double]
simulate p s = do
  let _i= ins2typeStr s
  aft <- MVec.replicate (nTask p) 0
  inss <- MVec.replicate (Vec.length _i) (-1)
  insf <- MVec.replicate (Vec.length _i) 0
  let ss = SS aft inss insf
  forM_ (orderStr s) $ scheduleTask p s ss
  makespan <- MVec.unsafeRead (_aft ss) (nTask p - 1)
  ssv <- Vec.freeze $ _inss ss
  ssf <- Vec.freeze $ _insf ss
  let ts = Vec.filter (\(_,x,_)->x>=0) $ Vec.zip3 _i ssv ssf
  return $ [makespan,  charge p . Account $ Vec.toList ts]

fromPool::[Task]->Vector Ins->Schedule
fromPool order locs =
  let _inss = Set.toList . Set.fromList $ Vec.toList locs
      index = Map.fromList $ zip _inss [0..]
      _t2i = Vec.map ((Map.!) index) $ locs
      _i2t = map (flip quot $ length order) _inss
  in Schedule order _t2i (Vec.fromList _i2t)

type2ins::Vec.Vector InsType->Int->Map.Map Ins Ins
type2ins ts n = Map.unions .
                map (assIns ts n) .
                groupBy ((==) `on` (ts!)) .
                sortBy (compare `on` (ts!)) $
                [0..Vec.length ts-1]

assIns::Vec.Vector InsType->Int->[Ins]->Map.Map Ins Ins
assIns ts n is = Map.fromList $
                 zipWith (\x y->(x, t*n+y)) is [0..]
  where t = ts ! head is

toPool::Problem->Schedule->(Orders, Vec.Vector Int)
toPool p (Schedule _o _t2i _i2t) = (_o, s)
    where m = type2ins _i2t $ nTask p
          s = flip Vec.map _t2i $ (Map.!) m
