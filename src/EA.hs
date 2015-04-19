module EA ( Population, EnvSelector, MutSelector, Breeder, PopInitialiser
          , EAToolbox (..)
          , EAConfig (..)
          , Individual (..)
          , Chromosome) where

import           Control.Monad        (join)
import           Control.Monad.Random (Rand, RandomGen)
import           Data.Function        (on)
import           Data.Functor         ((<$>))
import qualified Data.Vector          as Vec

import           MOP
import           Problem
import           Utils
import           Utils.Random         (withProb)

type Population c o = Vec.Vector (Individual c o)
type MutSelector g c o = Population c o->Rand g (Individual c o)
type EnvSelector c o = Population c o->Population c o->Population c o
type Breeder g c o = MutSelector g c o->Population c o->Population c o
type PopInitialiser c o = Problem->Int->Population c o

data EAToolbox g c o = EAToolbox { seeds   :: PopInitialiser c o
                                 , mutSel  :: MutSelector g c o
                                 , envSel  :: EnvSelector c o
                                 , breeder :: Breeder g c o}

data EAConfig = EAConfig { numGen  :: Int
                         , sizePop :: Int
                         , probCrs :: Double
                         , probMut :: Double}

type Individual = With

chrm::(Chromosome c)=>Individual c o->c
chrm (With c _) = c

objs::(Objectives o)=>Individual c o->o
objs (With _ o) = o

reproduce::(Chromosome c, Objectives o, RandomGen g)=>
           Problem->EAConfig->[Individual c o]->Rand g [Individual c o]
reproduce p c is = do cs' <- repChrm $ map chrm is
                      return . zipWith With cs' $
                        map (fromList . calObjs p . decode p) cs'
  where repChrm cs = do
          px <- withProb (probCrs c) (crossover p) return
          pm <- withProb (probMut c) (mutate p) return
          px cs >>= mapM pm

class Chromosome a where
  mutate::(RandomGen g)=>Problem->a->Rand g a
  crossover::(RandomGen g)=>Problem->[a]->Rand g [a]
  repMode::a->(Int, Int)

  decode::Problem->a->Schedule
  encode::Problem->Schedule->a

  mutate _ = return
  crossover _ = return
  repMode _ = (2, 1)
