{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module EA ( Population, EnvSelector, MutSelector, Breeder, PopInitialiser
          , normalBreeder
          , envSelectN
          , EAToolbox (..)
          , EASetup (..)
          , evalEA
          , Individual (..)
          , Chromosome (..)) where

import           Control.Monad        (join, replicateM)
import           Control.Monad.Random (Rand, RandomGen)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import qualified Data.Vector          as Vec

import           MOP                  (Objectives (..), WithObjs (..))
import           Problem
import           Utils
import           Utils.Random         (doWithProb)

type Population o c = Vec.Vector (Individual o c)
type MutSelector = (RandomGen g, WithObjs o)=>Vec.Vector o->Rand g o
type EnvSelector = (WithObjs o)=>Vec.Vector o->Vec.Vector o->Vec.Vector o
type Breeder = (RandomGen g, Chromosome c, Objectives o)=>Problem->
               EASetup->MutSelector->Population o c->Rand g (Population o c)
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
  show (Individual _ o) = show $ toList o

instance (Objectives o)=>WithObjs (Individual o c) where
  type Objs (Individual o c) = o
  getObjs = _objs

class Chromosome a where
  repMode::a->(Int, Int)
  crossover::(RandomGen g)=>Problem->[a]->Rand g [a]
  mutate::(RandomGen g)=>Problem->a->Rand g a

  decode::Problem->a->Schedule
  encode::Problem->Schedule->a

  mutate _ = return
  crossover _ = return
  repMode _ = (2, 1)

evalEA::(Chromosome c, Objectives o, RandomGen g)=>
        Problem->EASetup->EAToolbox->Rand g [Individual o c]
evalEA p c (EAToolbox _init _mSel _eSel _br) = do
  p0 <- Vec.map newInd <$> _init p (sizePop c)
  Vec.toList <$> foldM' doGen p0 [1..numGen c]
  where doGen pop _ = _eSel pop <$> _br p c _mSel pop
        newInd i = let c = encode p i
                       o = fromList $ calObjs p i
                   in Individual c o

normalBreeder::Breeder
normalBreeder p c sel is =
  Vec.fromList . concat <$> replicateM (div (sizePop c) nC) rp1
  where (nP, nC) = repMode . chrm $ Vec.head is
        rp1 = (replicateM nP $ sel is) >>= reproduce p c

reproduce::(Chromosome c, Objectives o, RandomGen g)=>
           Problem->EASetup->[Individual o c]->Rand g [Individual o c]
reproduce p c is = do cs' <- repChrm $ map chrm is
                      return . zipWith Individual cs' $
                        map (fromList . calObjs p . decode p) cs'
  where repChrm cs = do
          cs' <- join $ doWithProb (probCrs c) (crossover p) return cs
          mapM (join . doWithProb (probMut c) (mutate p) return) cs'

envSelectN::(WithObjs o)=>EnvSelector->Int->Vec.Vector o->Vec.Vector o
envSelectN sel n s = let (s0, s1) = Vec.splitAt n s
                     in sel s0 s1
