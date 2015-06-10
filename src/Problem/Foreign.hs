{-# LANGUAGE ForeignFunctionInterface #-}

module Problem.Foreign (setupProblem, finishProblem, computeObjs) where

import           Control.Monad         (forM_)
import qualified Data.Vector           as Vec
import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free)
import           Foreign.Marshal.Array (newArray)
import           Foreign.Marshal.Utils (new)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek)
import           System.IO.Unsafe      (unsafePerformIO)

import           Problem               (Problem, Schedule (..), cu, insPrice,
                                        nTask, nType, preds, refTime)

foreign import ccall "schedule_eval.h init_problem" initProblem::CInt->CInt->IO ()
foreign import ccall "schedule_eval.h finish_problem" finishProblem::IO()
foreign import ccall "schedule_eval.h setup_service" setupService::Ptr CDouble->Ptr CDouble->IO()
foreign import ccall "schedule_eval.h setup_tasks" setupTasks::Ptr CDouble->IO()
foreign import ccall "schedule_eval.h setup_preds" setupPreds::CInt->CInt->Ptr CInt->IO()
foreign import ccall "schedule_eval.h compute_objs" c_computeObjs::Ptr CInt->Ptr CInt->CInt->Ptr CInt->Ptr CDouble->Ptr CDouble->IO ()

setupProblem::Problem->IO()
setupProblem p = do
  -- Init Problem
  let nt = nTask p
      np = nType p
  initProblem (toEnum nt) (toEnum np)

  -- Setup Service
  prc <- newArray $ map (realToFrac . insPrice p) [0..np-1]
  cus <- newArray $ map (realToFrac . cu p) [0..np-1]
  setupService prc cus
  free prc
  free cus

  -- Setup Task
  ts <- newArray $ map (realToFrac . refTime p) [0..nt-1]
  setupTasks ts
  free ts

  -- Setup Preds
  forM_ [0..nt-1] $ \t->do let ps = preds p t
                           pds <- newArray $ map toEnum ps
                           setupPreds (toEnum t) (toEnum $ length ps) pds
                           free pds

computeObjs::Schedule->[Double]
computeObjs (Schedule o t2i i2t) = unsafePerformIO $ do
  o_p <- newArray $ map toEnum o
  t2i_p <- newArray . map toEnum $ Vec.toList t2i
  i2t_p <- newArray . map toEnum $ Vec.toList i2t
  m_p <- malloc
  c_p <- malloc

  c_computeObjs o_p t2i_p (toEnum $ Vec.length i2t) i2t_p m_p c_p

  m <- peek m_p
  c <- peek c_p

  free(c_p)
  free(m_p)
  free(i2t_p)
  free(t2i_p)
  free(o_p)

  return [realToFrac m, realToFrac c]
