{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import           Heuristic.Cheap               (cheap)
import           Heuristic.HBCS                (hbcs)
import           Heuristic.HEFT                (heft)

import           Problem                       (Problem (Prob), showObjs)
import           Problem.DAG.Pegasus           as Pegasus
import           Problem.DAG.Random            as RandDAG
import           Problem.Service.EC2           as EC2

import           System.Console.CmdArgs        (Data, Typeable, argPos, cmdArgs,
                                                def, help, name, summary, typ,
                                                typFile, (&=))
import           System.Random.Mersenne.Pure64 (newPureMT)

data Exp = Exp { alg   :: String
               , limit :: Int
               , kstep :: Double
               , file  :: [FilePath]} deriving (Data, Typeable, Show, Eq)

ea::Exp
ea = Exp { alg = "heft" &= argPos 0 &= typ "ALG"
         , limit = 10000 &=name "l" &= typ "MAX_INSTANCES"
                   &= help "Max instance limit."
         , kstep = 0.1 &= name "k" &= typ "NUM"
                   &= help "Execution number for heuristic."
         , file = def &= argPos 1 &= typFile
         } &= summary "Cloud Workflow Scheduling Experiment"

process::Exp->IO ()
process args = do
  w <- Pegasus.mkWorkflow . head $ file args
  s <- EC2.mkService $ limit args
  g <- newPureMT
  let p = Prob w s
  case alg $ args of
   "heft" -> mapM (putStrLn . showObjs p) [heft p]
   "cheap" -> mapM (putStrLn . showObjs p) [cheap p]
   "hbcs"   -> mapM (putStrLn . showObjs p . hbcs p) $ tail [0,kstep $ args..1]
  return ()

main = process =<< cmdArgs ea
