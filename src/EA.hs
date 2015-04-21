{-# LANGUAGE TypeFamilies #-}

module EA ( Population, EnvSelector, MutSelector, Breeder, PopInitialiser
          , normalBreeder
          , envSelectN
          , EAToolbox (..)
          , EAConfig (..)
          , evalEA
          , Individual (..)
          , Chromosome (..)) where

import           Control.Monad        (join, replicateM)
import           Control.Monad.Random (Rand, RandomGen, evalRand)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import qualified Data.Vector          as Vec

import           MOP                  (Objectives (..), WithObjs (..))
import           Problem
import           Utils
import           Utils.Random         (withProb)

type Population o c = Vec.Vector (Individual o c)
type MutSelector g o = Vec.Vector o->Rand g o
type EnvSelector o = Vec.Vector o->Vec.Vector o->Vec.Vector o
type Breeder g o c = Problem->EAConfig->
                     MutSelector g (Individual o c)->
                     Population o c->Rand g (Population o c)
type PopInitialiser g = Problem->Int->Rand g (Vec.Vector Schedule)

data EAToolbox g o c = EAToolbox { popInit :: PopInitialiser g
                                 , mutSel  :: MutSelector g (Individual o c)
                                 , envSel  :: EnvSelector (Individual o c)
                                 , breeder :: Breeder g o c}

data EAConfig = EAConfig { numGen  :: Int
                         , sizePop :: Int
                         , probCrs :: Double
                         , probMut :: Double}

data Individual o c = Individual { chrm  :: c
                                 , _objs :: o}

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
        Problem->EAConfig->EAToolbox g o c->g->[Individual o c]
evalEA p c (EAToolbox _init _mSel _eSel _br) g =
  flip evalRand g $ do p0 <- Vec.map newInd <$> _init p (sizePop c)
                       Vec.toList <$> foldM' doGen p0 [1..numGen c]
  where doGen pop _ = _eSel pop <$> _br p c _mSel pop
        newInd i = let c = encode p i
                       o = fromList $ calObjs p i
                   in Individual c o

normalBreeder::(Chromosome c, Objectives o, RandomGen g)=>Breeder g o c
normalBreeder p c sel is = Vec.fromList . concat <$>
                           (replicateM n $ replicateM nP s >>= reproduce p c)
  where (nP, nC) = repMode . chrm $ Vec.head is
        n = div (sizePop c) nC
        s = sel is

reproduce::(Chromosome c, Objectives o, RandomGen g)=>
           Problem->EAConfig->[Individual o c]->Rand g [Individual o c]
reproduce p c is = do cs' <- repChrm $ map chrm is
                      return . zipWith Individual cs' $
                        map (fromList . calObjs p . decode p) cs'
  where repChrm cs = do
          px <- withProb (probCrs c) (crossover p) return
          pm <- withProb (probMut c) (mutate p) return
          px cs >>= mapM pm

envSelectN::EnvSelector o->Int->Vec.Vector o->Vec.Vector o
envSelectN sel n s = let (s0, s1) = Vec.splitAt n s
                     in sel s0 s1
