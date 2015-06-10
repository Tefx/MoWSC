{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}


module EA ( Population, EnvSelector, MutSelector, Breeder, PopInitialiser
          , normalBreeder, normalBreederF
          , envSelectN
          , EAToolbox (..)
          , EASetup (..)
          , ExtraEAInfo
          , evalEA
          , Individual (..)
          , Chromosome (..)
          , NullInfo (..), EATrace (..)) where

import           Control.Monad        (join, replicateM)
import           Control.Monad.Random (Rand, RandomGen)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import           Data.List            (transpose)
import           Data.Vector          ((//))
import qualified Data.Vector          as Vec

import           MOP                  (ObjValue, Objectives (..), WithObjs (..))
import           Problem
import           Problem.Foreign      (computeObjs)
import           Utils
import           Utils.Random         (doWithProb)

type Population o c = Vec.Vector (Individual o c)
type MutSelector = (RandomGen g, WithObjs o)=>Vec.Vector o->Int->Rand g [o]
type EnvSelector = (WithObjs o)=>Vec.Vector o->Vec.Vector o->Vec.Vector o
type Breeder = (RandomGen g, Chromosome c, Objectives o)=>Problem->
               EASetup->Int->MutSelector->Population o c->Rand g (Population o c)
type PopInitialiser = (RandomGen g)=>Problem->Int->Rand g (Vec.Vector Schedule)

data EAToolbox = EAToolbox { popInit :: PopInitialiser
                           , mutSel  :: MutSelector
                           , envSel  :: EnvSelector
                           , breeder :: Breeder}

data EASetup = EASetup { numGen  :: Int
                       , sizePop :: Int
                       , probCrs :: Double
                       , probMut :: Double}

data Individual o c = Individual { chrm  :: c
                                 , _objs :: o}

instance (Objectives o)=>Show (Individual o c) where
  show x = "(" ++ (show $ _objs x) ++ ")"

instance (Objectives o)=>WithObjs (Individual o c) where
  type Objs (Individual o c) = o
  getObjs = _objs

class Chromosome a where
  repMode::a->(Int, Int)
  crossover::(RandomGen g)=>Problem->Double->[a]->Rand g [a]
  mutate::(RandomGen g)=>Problem->Double->a->Rand g a

  decode::Problem->a->Schedule
  encode::Problem->Schedule->a

  mutate _ _ = return
  crossover _ _ = return
  repMode _ = (2, 1)

class ExtraEAInfo a where
  empty::EASetup->a
  update::(Objectives o, Chromosome c)=>
          Problem->EASetup->Population o c->Int->a->a
  update _ _ _ _ = id

data NullInfo = NullInfo

instance ExtraEAInfo NullInfo where
  empty _ = NullInfo

newtype EATrace = EATrace {trace::Vec.Vector (Vec.Vector [ObjValue])}

instance ExtraEAInfo EATrace where
  empty c = EATrace . Vec.replicate (numGen c `quot` 10) $ Vec.singleton []
  update p c pop cur i@(EATrace trc)
    | cur `rem` 10 == 0 = let objs = Vec.map (toList . getObjs) pop
                          in EATrace $ trc // [(cur `quot` 10, objs)]
    | otherwise = i

evalEA::(Chromosome c, Objectives o, RandomGen g, ExtraEAInfo i)=>
        Problem->EASetup->EAToolbox->Rand g (With i (Population o c))
evalEA p c e = do p0 <- attach (empty c) . pureBulkEval p .
                        Vec.map (encode p) <$> (popInit e) p (sizePop c)
                  foldM' (evolve p c e) p0 [0..numGen c-1]

evolve::(ExtraEAInfo i, Objectives o, Chromosome c, RandomGen g)=>
        Problem->EASetup->EAToolbox->
        With i (Population o c)->Int->Rand g (With i (Population o c))

evolve p c (EAToolbox _ _mSel _eSel _br) wp cur =
  do let pop = original wp
     pop' <- _eSel pop <$> _br p c cur _mSel pop
     let info = update p c pop' cur $ attached wp
     return $! attach info pop'

normalBreeder::Breeder
normalBreeder p c cur mSel is =
  do s <- transpose <$> (replicateM nP $ mSel is (sizePop c `quot` nC))
     pureBulkEval p . Vec.fromList . concat <$> mapM (reproduce p c pg) s
  where (nP, nC) = repMode . chrm $ Vec.head is
        pg = (fromIntegral cur /) . fromIntegral $ numGen c

normalBreederF::Breeder
normalBreederF p c cur mSel is =
  do s <- transpose <$> (replicateM nP $ mSel is (sizePop c `quot` nC))
     cBulkEval p . Vec.fromList . concat <$> mapM (reproduce p c pg) s
  where (nP, nC) = repMode . chrm $ Vec.head is
        pg = (fromIntegral cur /) . fromIntegral $ numGen c

reproduce::(Chromosome c, RandomGen g)=>
           Problem->EASetup->Double->[Individual o c]->Rand g [c]
reproduce p c cur is = do
  let cs = map chrm is
  cs' <- join $ doWithProb (probCrs c) (crossover p cur) return cs
  mapM (join . doWithProb (probMut c) (mutate p cur) return) cs'

envSelectN::(WithObjs o)=>EnvSelector->Int->Vec.Vector o->Vec.Vector o
envSelectN sel n s = let (s0, s1) = Vec.splitAt n s
                     in sel s0 s1

pureEval::(Objectives o, Chromosome c)=>Problem->c->Individual o c
pureEval p i = Individual i . fromList . calObjs p $ decode p i

pureBulkEval::(Objectives o, Chromosome c)=>Problem->Vec.Vector c->Population o c
pureBulkEval p = Vec.map (pureEval p)

cEval::(Objectives o, Chromosome c)=>Problem->c->Individual o c
cEval p i = Individual i . fromList . computeObjs $ decode p i

cBulkEval::(Objectives o, Chromosome c)=>Problem->Vec.Vector c->Population o c
cBulkEval p = Vec.map (cEval p)
