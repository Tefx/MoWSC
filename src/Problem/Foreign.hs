{-# LANGUAGE ForeignFunctionInterface #-}

module Problem.Foreign (setupProblem, finishProblem) where

import           Control.Monad         (forM_)
import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free)
import           Foreign.Marshal.Array (newArray)
import           Foreign.Ptr           (Ptr)

import qualified Problem               as P

foreign import ccall "schedule_eval.h init_problem" initProblem::CInt->CInt->IO ()
foreign import ccall "schedule_eval.h finish_problem" finishProblem::IO()
foreign import ccall "schedule_eval.h setup_service" setupService::Ptr CDouble->Ptr CDouble->IO()
foreign import ccall "schedule_eval.h setup_tasks" setupTasks::Ptr CDouble->IO()
foreign import ccall "schedule_eval.h setup_preds" setupPreds::CInt->CInt->Ptr CInt->IO()

setupProblem::P.Problem->IO()
setupProblem p = do

  -- Init Problem
  let nt = P.nTask p
      np = P.nType p
  initProblem (toEnum nt) (toEnum np)

  -- Setup Service
  prc <- newArray $ map (realToFrac . P.insPrice p) [0..np-1]
  cus <- newArray $ map (realToFrac . P.cu p) [0..np-1]
  setupService prc cus
  free prc
  free cus

  -- Setup Task
  ts <- newArray $ map (realToFrac . P.refTime p) [0..nt-1]
  setupTasks ts
  free ts

  -- Setup Preds
  forM_ [0..nt-1] $ \t->do let ps = P.preds p t
                           pds <- newArray $ map toEnum ps
                           setupPreds (toEnum t) (toEnum $ length ps) pds
                           free pds
