{-# LANGUAGE RankNTypes #-}

module EA.PSO (Particle) where

import           EA
import           MOP                  (Objectives, getObjs, (<<<))
import           Problem              (Orders, fromPool, nTask, nType, toPool)

import           Control.DeepSeq      (NFData (..))
import           Control.Monad        (replicateM)
import           Control.Monad.Random (Rand, RandomGen, getRandomR)
import           Data.Functor         ((<$>))
import qualified Data.Vector          as Vec

import Debug.Trace

type Position = Vec.Vector Int
type Velocity = Vec.Vector Double

item::Position->Int->Int->Double
item p m i = if (p Vec.! x) == y then 1 else 0
  where x = i `quot` m
        y = i `rem` m

getParameters::Double->(Double, Double, Double)
getParameters c = (w_max - c * (w_max - w_min), c1, c2)
  where w_max = 1.0
        w_min = 0.4
        c1 = 2
        c2 = 2

updateVelItem::(Double, Double, Double)->Double->Double->
               Int->Position->Position->Position->
               Int->Double->Double
updateVelItem (w, c1, c2) r1 r2 m gbest pbest x i v =
  w * v +
  c1 * r1 * (item pbest m i - item x m i) +
  c2 * r2 * (item gbest m i - item x m i)

updateVel::(RandomGen g)=>Double->Int->
                  Position->Position->Position->Velocity->Rand g Velocity
updateVel c m gbest pbest x v = do
  r1 <- getRandomR (0, 1)
  r2 <- getRandomR (0, 1)
  return $ Vec.imap (updateVelItem (getParameters c) r1 r2 m gbest pbest x) v

pos4i::Int->Velocity->Int->Int
pos4i m v i = Vec.maxIndex $ Vec.slice (i*m) m v

nextPosition::Int->Int->Velocity->Position
nextPosition n m v =  Vec.generate n $ pos4i m v

data Particle = Particle { pos   :: Position
                         , vel      :: Velocity
                         , _order   :: Orders}

instance NFData Particle where
  rnf (Particle a b c) = rnf a `seq` rnf b `seq` rnf c

instance Chromosome Particle where
  repMode _ = (3, 1)

  crossover p c [Particle gP _ _, Particle pP _ _, Particle cP vel o] =
    do vel' <- updateVel c m gP pP cP vel
       let pos' = nextPosition n m vel'
       return $ [Particle pos' vel' o]
    where n = nTask p
          m = nTask p * nType p

  encode p s = do let (o, str) = toPool p s
                      num = nTask p * nTask p * nType p
                  vel <- Vec.replicateM num $ getRandomR (0, 1)
                  return $ Particle str vel o

  decode _ i = fromPool (_order i) (pos i)
