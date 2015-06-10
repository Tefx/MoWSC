{-# LANGUAGE ForeignFunctionInterface #-}

module Problem.ForeignEvaluation () where

import qualified Data.Vector.Storable as VS
import           Foreign                 (Storable (..), Ptr)
import           Foreign.C.Types

import qualified Problem              as P

data CPreds = CPreds { ps :: Ptr CInt
                     , np :: CInt}

data CProblem = CProblem { nTask   : : CInt
                         , nType   :: CInt
                         , refTime :: Ptr CFloat
                         , preds   :: Ptr CPreds
                         , prices  :: Ptr CFloat
                         , cus     :: Ptr CFloat}

data CSchedule = CSchedule { order    :: Ptr CInt
                           , task2ins :: Ptr CInt
                           , ins2type :: Ptr CInt
                           , nIns     :: CInt}

data CObjs = CObjs { makespan :: CFloat
                   , cost     :: CFloat}

{-
toCPreds::P.Problem->Int->CPreds
toCPreds p i = let s = VS.fromList $ P.preds p i
                   n = VS.length s
               in CPreds s n

toCProb::P.Problem->CProblem
toCProb p = let n = P.nTask p
                rt = VS.generate n (P.refTime p)
                pd = VS.generate n (toCPreds p)
                prs = VS.generate n (P.insPrice p)
                cus = VS.generate n (P.cu p)
            in CProblem n (P.nType p) rt pd prs cus

toSchd::P.Schedule->CSchedule
toSchd s = let o = VS.fromList $ P.orderStr s
               t2i = VS.convert $ P.task2insStr s
               i2t = VS.convert $ P.ins2typeStr s
           in CSchedule o t2i i2t $ VS.length i2t

fromCObjs::CObjs->(Double, Double)
fromCObjs (CObjs m c) = (m, c)
--}

-- For C

#include "schedule_eval.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable CPreds where
  sizeOf _ = #size Preds
  alignment _ = #alignment Preds
  peek ptr = do
    s <- #{peek Preds, pds} ptr
    n <- #{peek Preds, np} ptr
    return $ CPreds s n
  poke ptr (CPreds s n) = do
    #{poke Preds, pds} ptr s
    #{poke Preds, np} ptr n

instance Storable CProblem where
  sizeOf _ = #size Problem
  alignment _ = #alignment Problem
  peek ptr = do
    n <- #{peek Problem, n_task} ptr
    nt <- #{peek Problem, n_type} ptr
    rt <- #{peek Problem, reft} ptr
    pd <- #{peek Problem, preds} ptr
    pi <- #{peek Problem, prices} ptr
    cu <- #{peek Problem, cus} ptr
    return $ CProblem n nt rt pd pi cu
  poke ptr (CProblem n nt rt pd pi cu) = do
    #{poke Problem, n_task} ptr n
    #{poke Problem, n_type} ptr nt
    #{poke Problem, reft} ptr rt
    #{poke Problem, preds} ptr pd
    #{poke Problem, prices} ptr pi
    #{poke Problem, cus} ptr cu

instance Storable CSchedule where
  sizeOf _ = #size Schedule
  alignment _ = #alignment Schedule
  peek ptr = do
    o <- #{peek Schedule, order} ptr
    t2i <- #{peek Schedule, t2i} ptr
    i2t <- #{peek Schedule, i2t} ptr
    ni  <- #{peek Schedule, n_ins} ptr
    return $ CSchedule o t2i i2t ni
  poke ptr (CSchedule o t2i i2t ni) = do
    #{poke Schedule, order} ptr o
    #{poke Schedule, t2i} ptr t2i
    #{poke Schedule, i2t} ptr i2t
    #{poke Schedule, n_ins} ptr ni

instance Storable CObjs where
  sizeOf _ = #size Objs
  alignment _ = #alignment Objs
  peek ptr = do
    m <- #{peek Objs, makespan} ptr
    c <- #{peek Objs, cost} ptr
    return $ CObjs m c
  poke ptr (CObjs m c) = do
    #{poke Objs, makespan} ptr m
    #{poke Objs, cost} ptr c
