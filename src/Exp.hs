{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import           EA                            (EAConfig (..), EAToolbox (..),
                                                evalEA, normalBreeder)
import           EA.Chromosome                 (C0, C2)
import           EA.Init                       (randInsOrTypeOrHEFT, randPool)
import           EA.NSGA2                      (nsga2Select)
import           EA.Selection                  (tournamentSelGen)
import           EA.SPEA2                      (spea2Select)
import           Heuristic.Cheap               (cheap)
import           Heuristic.HBCS                (hbcs)
import           Heuristic.HEFT                (heft)
import           Heuristic.MOHEFT              (moheft)
import           MOP                           (MakespanCost, getObjs)
import           Problem                       (Problem (Prob), showObjs)
import           Problem.DAG.Pegasus           as Pegasus
import           Problem.DAG.Random            as RandDAG
import           Problem.Service.EC2           as EC2

import           Control.Monad.Random          (RandomGen)
import           System.Console.CmdArgs        (Data, Typeable, argPos, cmdArgs,
                                                def, help, name, summary, typ,
                                                typFile, (&=))
import           System.Random.Mersenne.Pure64 (PureMT, newPureMT)

data Exp = Exp { alg     :: String
               , limit   :: Int
               , kstep   :: Double
               , popsize :: Int
               , gen     :: Int
               , pcr     :: Double
               , pmu     :: Double
               , file    :: [FilePath]} deriving (Data, Typeable, Show, Eq)

ea::Exp
ea = Exp { alg = "heft" &= argPos 0 &= typ "ALG"
         , limit = 10000 &=name "l" &= typ "MAX_INSTANCES"
                   &= help "Max instance limit."
         , kstep = 0.1 &= name "k" &= typ "NUM"
                   &= help "Execution number for heuristic."
         , popsize = 50 &= name "p" &= typ "NUM"
                     &= help "Size of Population."
         , gen = 1000 &= name "g" &= typ "NUM"
                 &= help "Number of Generation."
         , pcr = 1 &= name "c" &= typ "NUM"
                 &= help "prob of Crossover."
         , pmu = 1 &= name "m" &= typ "NUM"
                 &= help "prob of Mutation."
         , file = def &= argPos 1 &= typFile
         } &= summary "Cloud Workflow Scheduling Experiment"

process::Exp->IO ()
process args = do
  w <- Pegasus.mkWorkflow . head $ file args
  s <- EC2.mkService $ limit args
  g <- newPureMT
  let p = Prob w s
      ec = EAConfig { numGen = gen $ args
                    , sizePop = popsize $ args
                    , probCrs = pcr $ args
                    , probMut = pmu $ args}
      et0 = EAToolbox { popInit = randPool
                      , mutSel = tournamentSelGen
                      , envSel = nsga2Select
                      , breeder = normalBreeder} :: (RandomGen g)=>EAToolbox g MakespanCost C0
      et2 = EAToolbox { popInit = randInsOrTypeOrHEFT
                      , mutSel = tournamentSelGen
                      , envSel = spea2Select
                      , breeder = normalBreeder} :: (RandomGen g)=>EAToolbox g MakespanCost C2
  case alg $ args of
   "heft"   -> mapM (putStrLn . showObjs p) [heft p]
   "cheap"  -> mapM (putStrLn . showObjs p) [cheap p]
   "hbcs"   -> mapM (putStrLn . showObjs p . hbcs p) $ tail [0,kstep $ args..1]
   "moheft" -> mapM (putStrLn . showObjs p) . moheft p $ popsize args
   "ea0"    -> mapM (putStrLn . show . getObjs) $ evalEA p ec et0 g
   "ea2"    -> mapM (putStrLn . show . getObjs) $ evalEA p ec et2 g
  return ()

main = process =<< cmdArgs ea



{- Made by
QQQQQQQQQQQQQQQQQQQQQWT????WQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQP^ayQW?QQQQQQQQQQ
QQQQQQQQQQQQQQQQQQQ@"swQQQQw74QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ?\aQQQQQ 4QQQQQQQQQ
QQQQQQQQQQQQQQQQQQP_mQQQQQQk4a?QQQQQQQQQQQQQQQQQQQQQQQQQQQ@?_wQWQQQQQ ]QQQQQQQQQ
QQQQQQQQQQQQQQQQQP_QQQQQQQQQ.46"$QQQQQQQQQQQQQQQQQQQQQQQ@!.wWQQQQQQQQ ]QQQQQQQQQ
QQQQQQQQQQQQQQQQP yQQQQQQQQQ[+Q6-$QQQQQQQQQQQQQQQQQQQQQP`_QWQQQQQQQQQ  4QQQQQQQQ
QQQQQQQQQQQQQQQW`jQQQQQQQQQQm $Qg.$QQQQQQQQQQQQQQQQQQQP jQQQQQQQQQQQQ  jQQQQQQQQ
QQQQQQQQQQQQQQQ(.QQQQQQQQQQQQ,]QQg;$QQQQQQQQQWBBBWQQ@?_mQQQQQQQQQQQQB  QWQQQQQQQ
QQQQQQQQQQQQQQP jQQQQQQQQQQQQL-QQQm/9VT?!isaaawwwaaaamQQDT9QW$P]QQQQF ]QQQQQQQQQ
QQQQQQQQQQQQQQ'=QQQQQQQQQQQQQQ $QQQQQmWQQQWWWWWWWQQWWQQQQQw/"?.mQQQQ[ mQQQQQQQQQ
QQQQQQQQQQQQQ@ jQQQQQQQQQQQQQWcjQQQQQWQQQQQQQQQQQQQQQQQQQWWQQa;$QQQQ`<QQQQQQQQQQ
QQQQQQQQQQQQQ[.QQQQQQQ@9WQQQTWWWQQQQQQQQQQQQD:QQQQQQQQQQQQQQQQQw7$QE jQQQQQQQQQQ
QQQQQQQQQQQQQ`]QQQQQWQQ WW?:wWQQQQQQQQQ@QWQ!$QsmHWWQQQQQQQQQQQQQQa"(_QQQQQQQQQQQ
QQQQQQQQQQQQ@ mQQQQQg/4w?<wQQQQQQQQQQQQQWQQwWQWQc-QQQQQQQQQQQQQQQQg.]QQQQQQQQQQQ
QQQQQQQQQQQQf.QQQQQQQQ`_yQQQQQQQQQQQQQQQQQWyWQQWm jQQQDWQQQWQQQQQWQm-WQQQQQQQQQQ
QQQQQQQQQQQQ[]QQQQQQP=wQQQQQQQQQQQQQQQQQQQQWWQQQQ/mQQFjWQW4QQQQQWQQQ[]QQQQQQQQQQ
QQQQQQQQQQQQ'jQQQQ@^aWQQQQQQQQQQQQQQQQQQQQ@4WQQQQQQQDjWWQQQQQQQQQQQQQ QQQQQQQQWQ
QQQQQQQQQQQQ`mQQQQmyQQQQQQQQQQQQQQQQQQQQQQk QQQQQP4@_QWQ(dQQQQQQQQQQW;]QQQQQQQQQ
QQQQQQQQQQQW QQQ!jQQQQQQQQQQQQQQQQQwQQQQQQQamQQQQkjQQQQQQQQQQQP\QQQQQ[]QQQQQQQQQ
QQQQQQQQQQQE QW'jQQQQQQQQQQQQQQQQQQWQQQQQQW@QQQQQQQQQQQQQQQQQmwQQP]QQ[)WQQQQQQQQ
QQQQQQQQQQQk.@'yQQQQQQQQQWQQQQQQQQQQWVTVQWQW4QQQQDQQQ!_wwQW9QWWQQwmQQ[)WQQQQQQQQ
QQQQQQQQQQQk \mQQQQQQwwQQQQQQQQQQQQ?L<mga/4D.QQQQ QQ(<QQWWWmVQQQQQWQQ']WQQQQQQQQ
QQQQQQQQQQQQwQQQQQQQQQQQQQQQQQQWWQQwjQQQWQm/.QQQW Q(_QQQQQ@'vWQWQQQQQ jWQQQQQQQQ
QQQQQQQQQQQQWQQQQQQQQQQQQQQQWQQQa)QQk?QQQQQWwQQQ#_f_QQQQQT.yWQQQQQQQ@ QWQQQQQQQQ
QQQQQQQQQQQQQQQQQQQQQQQQQQQWQQQWWQQQQL+$WQQQkWQQQD_QQQWT:aQWQQQWWQQQf_QQQQQQQQQQ
QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ?"oY?9ma?WWQQQQQQgmWT"saT!>4WW\mWQQW`yQQQQQQQQQQ
QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ,]QFjc-QWmQQQQQQQWQawQ'_,-'jWQQQQQQ[<QQQQQQQQQQQ
QQQQQQQQQQQQQQQQQQQQQQQQQQW?$QQQQwaY?_aQQQQQQQQQWmQD?QQgwaQWWQQQQQP.QQQQQQQQQQQQ
QQQQQQQQQQQQQQQQQQQQWQQQQWQQgwT9QWQQQQQQQmyQQQQQQQQQQWQQWD9D9V1QQP jQQQQQQQQQQQQ
QQQQQQQQQQP!QQPWWQQQQQQQQQQQQWQQgQQQQQQ&amQQQQQQQQQQQQQQZYTVVHQQD jQQQQQQQQQQQQQ
QQQQQQQQP'aQWQQmQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQWQQQQWWP <QQQQQQQQQQQQQQ
QQQQQQQ!a$QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQWQP <QQQQQQQQQQQQQQQ
QQQQQQmP_QQQwWQQQQQ@$QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQP <QQQQQQQQQQQQQQQQ
QQQQQQf T{QQmQQQQQWmaYWQQQWQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQP <D4QQQQQQQQQQQQQQQ
QQQQQF_mQQQQQQQQQQQQQQmQQQQQQQQWQQQWQQQQQQQQQQQQQQQQQQQQQQQP <QQgwTQQQQQQQQQQQQQ
QQQQD mQgQWQ#aD$WQQQ@T?????9$QQQQQQQQQQQQQQQQQQQQQQQQQQQQQP -QQ@wmQQBY??9QQQQQQQ
QQQQ(]Q@YdQQT\aQWW?=samQQQQmw%?QWQQQQQQQQQWVVVT$QHWQQQQQQP mmQQQWQQQQWQQwa)9QQQQ
QQQW dmyWQQgmWWQP`wWWQQQQQQQWQm/4WQQQQQQW[<ymmQQmyQWQQQWP /$QQQQQQQQQQQWQWQa)QQB
QQQ#_QQQQQQWQWQP mQQQQQQQQQQQQQm -4WQQQQQmc"!w??\QWQQWP'_,_QQQQQQQQQQQQQQQQQr]Qm
QQQQQQQQQQWQQQQL=QQQQQQQQQQQQQQD_Qw,?9QQQQQQwaawWwT?"saQQ.jQQQQQQQQQQQQQ@WQQL)WQ
QQQQQQQQQQQQQQQQgQZ!"!"*||<aa><amQWQmw%>???T???!iaamQQQQQL)$QQ8THWQ@"HQWmUHU'jQQ
-}
