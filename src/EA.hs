{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}


module EA ( Population, EnvSelector, MutSelector, Breeder, PopInitialiser
          , normalBreeder
          , envSelectN
          , EAToolbox (..)
          , EASetup (..)
          , ExtraEAInfo
          , evalEA
          , Individual (..)
          , Chromosome (..)
          , NullInfo (..)) where

import           Control.Monad        (join, replicateM)
import           Control.Monad.Random (Rand, RandomGen)
import           Data.Aeson           (ToJSON)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import           Data.List            (transpose)
import qualified Data.Vector          as Vec
import           GHC.Generics         (Generic)

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
  crossover::(RandomGen g)=>Problem->[a]->Rand g [a]
  mutate::(RandomGen g)=>Problem->a->Rand g a

  decode::Problem->a->Schedule
  encode::Problem->Schedule->a

  mutate _ = return
  crossover _ = return
  repMode _ = (2, 1)

class ExtraEAInfo a where
  empty::a
  update::(Objectives o, Chromosome c)=>
          Problem->EASetup->Population o c->Int->a->a
  update _ _ _ _ = id

data NullInfo = NullInfo --deriving (Show, Generic)

--instance ToJSON NullInfo

instance ExtraEAInfo NullInfo where
  empty = NullInfo

evalEA::(Chromosome c, Objectives o, RandomGen g, ExtraEAInfo i)=>
        Problem->EASetup->EAToolbox->Rand g (With i (Population o c))
evalEA p c e = do let  newInd i = let c = encode p i
                                      o = fromList $ calObjs p i
                                  in Individual c o
                  p0 <- attach empty . Vec.map newInd <$>
                        (popInit e) p (sizePop c)
                  foldM' (evolve p c e) p0 [1..numGen c]

evolve::(ExtraEAInfo i, Objectives o, Chromosome c, RandomGen g)=>
        Problem->EASetup->EAToolbox->
        With i (Population o c)->Int->Rand g (With i (Population o c))

evolve p c (EAToolbox _ _mSel _eSel _br) wp cur =
  do let pop = original wp
     pop' <- _eSel pop <$> _br p c _mSel pop
     let info = update p c pop' cur $ attached wp
     return $! attach info pop'

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
