{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

{- Author
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

module Main (main) where

import           EA                            (Chromosome, EASetup (..),
                                                EAToolbox (..), EATrace (..),
                                                ExtraEAInfo, Individual,
                                                NullInfo (..), PopInitialiser,
                                                Population, abcBreeder, evalEA,
                                                normalBreeder, normalBreederF)
import           EA.Chromosome
import qualified EA.Foreign                    as EF
import           EA.Init
import           EA.NSGA2                      (assignNSGA2Fit, nsga2Select)
import           EA.Selection                  (nullSelGen, rouletteSelGen,
                                                tournamentSelGen)
import           EA.SPEA2                      (assignSPEA2Fit, spea2Select)
import           Heuristic.BHEFT               (bheft)
import           Heuristic.Cheap               (cheap)
import           Heuristic.EBCS                (ebcs)
import           Heuristic.HBCS                (hbcs)
import           Heuristic.HEFT                (heft)
import           Heuristic.LBCS                (lbcs)
import           Heuristic.MBCS                (mbcs)
import           Heuristic.MLS                 (mls)
import           Heuristic.MOHEFT              (moheft)
import           Misc.LOSS                     (loss2, loss3)
import           MOP                           (MakespanCost, ObjValue,
                                                Objectives, getObjs, toList)
import           Problem                       (Problem (Prob), Schedule,
                                                calObjs, nTask, nType)
import           Problem.DAG.TinyDAG           as DAG
import           Problem.Service.EC2           as EC2
import           Utils                         (With, attached, original)

import           Control.Monad.Random          (Rand, RandomGen, evalRand)
import           Data.Aeson                    (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.Vector                   as Vec
import           GHC.Generics                  (Generic)
import           Problem.Foreign               (computeObjs, finishProblem,
                                                setupProblem)
import           Random.Xorshift               (newXorshift)
import           System.Console.CmdArgs        (Data, Typeable, argPos, cmdArgs,
                                                def, help, name, summary, typ,
                                                typFile, (&=))
import           System.Random.Mersenne.Pure64 (PureMT, newPureMT)

data Exp = Exp { alg     :: String
               , limit   :: Int
               , popsize :: Int
               , gen     :: Int
               , pcr     :: Double
               , pmu     :: Double
               , file    :: [FilePath]} deriving (Data, Typeable, Show, Eq)

ea::Exp
ea = Exp { alg = "heft" &= argPos 0 &= typ "ALG"
         , limit = 10000 &=name "l" &= typ "MAX_INSTANCES"
                   &= help "Max instance limit."
         , popsize = 20 &= name "p" &= typ "NUM"
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
  w <- DAG.fromFile . head $ file args
  s <- EC2.mkService $ limit args
  -- g <- newPureMT
  g <- newXorshift
  let p = Prob w s
      ec = EASetup { numGen = gen $ args
                   , sizePop = popsize $ args
                   , probCrs = pcr $ args
                   , probMut = pmu $ args}
  setupProblem p
  BL.putStrLn $
    case alg $ args of
    "heft"     -> dumpRes . (NullInfo,) $ map computeObjs [heft p]
    "cheap"    -> dumpRes . (NullInfo,) $ map computeObjs [cheap p]
    "loss3"    -> runBudgetHeuristic p (popsize args) loss3
    "loss2"    -> runBudgetHeuristic p (popsize args) loss2
    "bheft"    -> runBudgetHeuristic p (popsize args) bheft
    "hbcs"     -> runBudgetHeuristic p (popsize args) hbcs
    "ebcs"     -> runBudgetHeuristic p (popsize args) ebcs
    "lbcs"     -> runBudgetHeuristic p (popsize args) lbcs
    "mbcs"     -> runBudgetHeuristic p (popsize args) mbcs
    "moheft"   -> dumpRes . (NullInfo,) . map computeObjs . moheft p $ popsize args
    "mls"      -> dumpRes . (NullInfo,) . map computeObjs . mls p $ popsize args

    "nsga2_c3" -> dumpRes . runEA g $ eaNSGA2_C3 p ec
    "spea2_c0" -> dumpRes . runEA g $ eaSPEA2_C0 p ec
    "spea2_c3" -> dumpRes . runEA g $ eaSPEA2_C3 p ec
    "spea2_c5" -> dumpRes . runEA g $ eaSPEA2_C5 p ec
    "moabc" -> dumpRes . runEA g $ eaMOABC p ec
  finishProblem


main = process =<< cmdArgs ea

data ResWithInfo i = RI { results :: [[ObjValue]]
                        , extra   :: i} deriving (Show, Generic)

instance (ToJSON i)=>ToJSON (ResWithInfo i)

dumpRes::(ToJSON i)=>(i,[[ObjValue]])->BL.ByteString
dumpRes (i,res) = encode $ RI res i

deriving instance Generic NullInfo
deriving instance Show NullInfo
instance ToJSON NullInfo

deriving instance Generic EATrace
deriving instance Show EATrace
instance ToJSON EATrace

type ExpType o c = (RandomGen g)=>
                   Problem->EASetup->Rand g (With EATrace (Population o c))

runEA::(Objectives o, RandomGen g, ExtraEAInfo i)=>g->
       Rand g (With i (Population o c))->(i, [[ObjValue]])
runEA g is = let res = evalRand is g
                 info = attached res
                 pop = map (toList . getObjs) . Vec.toList $ original res
             in (info, pop)

nsga2::(Objectives o, Chromosome c)=>PopInitialiser->ExpType o c
nsga2 i p c = evalEA p c $ EAToolbox { popInit = i
                                     , mutSel = tournamentSelGen
                                     , envSel = nsga2Select
                                     , breeder = normalBreederF}

spea2::(Objectives o, Chromosome c)=>PopInitialiser->ExpType o c
spea2 i p c = evalEA p c $ EAToolbox { popInit = i
                                     , mutSel = nullSelGen
                                     , envSel = EF.spea2Select
                                     , breeder = normalBreederF}

abc::(Objectives o, Chromosome c)=>PopInitialiser->ExpType o c
abc i p c = evalEA p c' $ EAToolbox { popInit = i
                                    , mutSel = rouletteSelGen assignSPEA2Fit
                                    , envSel = EF.spea2Select
                                    , breeder = abcBreeder}
  where c' = c{numGen=numGen c `quot` 2}

eaNSGA2_C3::ExpType MakespanCost C3
eaNSGA2_C3 = nsga2 randTypeSRH

eaSPEA2_C0::ExpType MakespanCost C0
eaSPEA2_C0 = spea2 randPoolOrHeft

eaSPEA2_C3::ExpType MakespanCost C3
eaSPEA2_C3 = spea2 randTypeSRH

eaSPEA2_C5::ExpType MakespanCost C5
eaSPEA2_C5 = spea2 randTypeSRH

eaMOABC::ExpType MakespanCost C0
eaMOABC = abc randPoolOrHeft

calBudget::Problem->Double->Double
calBudget p k = let c_max = (!!1) . computeObjs $ heft p
                    c_min = (!!1) . computeObjs $ cheap p
                in c_min + k * (c_max - c_min)

data BudgetInfo = BI { budgets  :: [Double]
                     , heftRes  :: [Double]
                     , cheapRes :: [Double]} deriving (Generic, Show)

instance ToJSON BudgetInfo

runBudgetHeuristic::Problem->Int->(Problem->Double->Schedule)->BL.ByteString
runBudgetHeuristic p n alg =
  let heftRes@([_, c_max]) = computeObjs $ heft p
      cheapRes@([_, c_min]) =  computeObjs $ cheap p
      bs = [c_min + (fromIntegral x/fromIntegral n) * (c_max-c_min) | x<-[1..n]]
      rs = map (computeObjs . alg p) bs
  in encode . RI rs $ BI bs heftRes cheapRes
