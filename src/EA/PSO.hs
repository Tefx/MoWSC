{-# LANGUAGE RankNTypes #-}

module EA.PSO (Particle) where

import           EA
import           MOP                  (Objectives, getObjs, (<<<))
import           Problem              (Orders, fromPool, nTask, nType, toPool)

import           Control.DeepSeq      (NFData (..))
import           Control.Monad        (replicateM)
import           Control.Monad.Random (Rand, RandomGen, getRandomR)
import           Data.Functor         ((<$>))
import qualified Data.Vector.Unboxed  as Vec
import           GHC.Float

import           Debug.Trace

type Position = Vec.Vector Int
type Velocity = Vec.Vector Float

item::Position->Int->Int->Float
{-#INLINE item#-}
item p m i = if (Vec.unsafeIndex p $ i `quot` m) == i `rem` m then 1 else 0

getParameters::Double->(Float, Float, Float)
getParameters c = let w_max = 1.0
                      w_min = 0.4
                      c1 = 2
                      c2 = 2
                  in (w_max - double2Float c * (w_max - w_min), c1, c2)

updateVelItem::(RandomGen g)=>(Float, Float, Float)->
               Int->Position->Position->Position->
               Int->Float->Rand g Float
updateVelItem (w, c1, c2) m gbest pbest p i v =
  do let x = item pbest m i - item p m i
     x0 <- if x == 0 then return 0
           else (c1 * x *) <$> getRandomR (0, 1)
     let y = item gbest m i - item p m i
     y0 <- if y == 0 then return 0
           else (c2 * y *) <$> getRandomR (0, 1)
     return $ w * v + x0 + y0


updateVel::(RandomGen g)=>Double->Int->
                  Position->Position->Position->Velocity->Rand g Velocity
updateVel c m gbest pbest x v =
  Vec.imapM (updateVelItem (getParameters c) m gbest pbest x) v

pos4i::Int->Velocity->Int->Int
pos4i m v i = Vec.maxIndex $ Vec.slice (i*m) m v

nextPosition::Int->Int->Velocity->Position
nextPosition n m v =  Vec.generate n $ pos4i m v

data Particle = Particle { pos    :: Position
                         , vel    :: Velocity
                         , _order :: Orders}

instance NFData Particle where
  rnf (Particle a b c) = rnf a `seq` rnf b `seq` rnf c

instance Chromosome Particle where
  repMode _ = (3, 1)

  crossover p c [Particle gP _ _, Particle pP _ _, Particle cP vel o] =
    do vel' <- updateVel c m gP pP cP vel
       return $ [Particle (nextPosition n m vel') vel' o]
    where n = nTask p
          m = n * nType p

  encode p s = do let (o, str) = toPool p s
                      num = nTask p * nTask p * nType p
                      --vel = Vec.replicate num 0
                  vel <- Vec.replicateM num $ getRandomR (-1, 1)
                  return $ Particle (Vec.convert str) vel o

  decode _ i = fromPool (_order i) (Vec.convert $ pos i)
