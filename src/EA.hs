{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module EA ( Population, EnvSelector, MutSelector, Breeder, PopInitialiser
          , normalBreeder, normalUpdater
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
import           Data.List            (transpose)
import qualified Data.Vector          as Vec

import           MOP                  (Objectives (..), WithObjs (..))
import           Problem
import           Utils
import           Utils.Random         (doWithProb)

type Population o c = Vec.Vector (Individual o c)
type MutSelector = (RandomGen g, WithObjs o)=>Vec.Vector o->Int->Rand g [o]
type EnvSelector = (WithObjs o)=>Vec.Vector o->Vec.Vector o->Vec.Vector o
type Breeder = (RandomGen g, Chromosome c, Objectives o)=>Problem->
               EASetup->MutSelector->Population o c->Rand g (Population o c)
type PopInitialiser = (RandomGen g)=>Problem->Int->Rand g (Vec.Vector Schedule)
type PopUpdater = (Objectives o, Chromosome c)=>EAToolbox->
                  With a (Population o c)->(Population o c)->With a (Population o c)

data EAToolbox = EAToolbox { popInit :: PopInitialiser
                           , mutSel  :: MutSelector
                           , envSel  :: EnvSelector
                           , breeder :: Breeder
                           , popUpd  :: PopUpdater}

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
  crossover::(RandomGen g)=>Problem->[a]->Rand g [a]
  mutate::(RandomGen g)=>Problem->a->Rand g a

  decode::Problem->a->Schedule
  encode::Problem->Schedule->a

  mutate _ = return
  crossover _ = return
  repMode _ = (2, 1)

evalEA::(Chromosome c, Objectives o, RandomGen g)=>
        Problem->EASetup->a->EAToolbox->Rand g (With a (Population o c))
evalEA p c a e@(EAToolbox _init _mSel _eSel _br _upd) =
  let doGen !pi _ = do pop' <- _br p c _mSel $ original pi
                       return $ _upd e pi pop'
      newInd i = let c = encode p i
                     o = fromList $ calObjs p i
                 in Individual c o
  in do p0 <- Vec.map newInd <$> _init p (sizePop c)
        foldM' doGen (attach a p0) [1..numGen c]

normalUpdater::PopUpdater
normalUpdater e s np = attach (attached s) $ (envSel e) (original s) np

normalBreeder::Breeder
normalBreeder p c mSel is =
  do s <- transpose <$> (replicateM nP $ mSel is (sizePop c `quot` nC))
     Vec.fromList . concat <$> mapM (reproduce p c) s
  where (nP, nC) = repMode . chrm $ Vec.head is

reproduce::(Chromosome c, Objectives o, RandomGen g)=>
           Problem->EASetup->[Individual o c]->Rand g [Individual o c]
reproduce p c is =
  let repChrm cs = do
        cs' <- join $ doWithProb (probCrs c) (crossover p) return cs
        mapM (join . doWithProb (probMut c) (mutate p) return) cs'
  in do cs' <- repChrm $ map chrm is
        return . zipWith Individual cs' $
          map (fromList . calObjs p . decode p) cs'

envSelectN::(WithObjs o)=>EnvSelector->Int->Vec.Vector o->Vec.Vector o
envSelectN sel n s = let (s0, s1) = Vec.splitAt n s
                     in sel s0 s1
