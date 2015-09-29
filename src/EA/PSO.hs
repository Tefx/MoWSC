{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes               #-}

module EA.PSO (Particle) where

import           EA
import           MOP                          (Objectives, getObjs, (<<<))
import           Problem                      (Orders, fromPool, nTask, nType,
                                               toPool)

import           Control.DeepSeq              (NFData (..))
import           Control.Monad                (replicateM)
import           Control.Monad.Random         (Rand, RandomGen, getRandomR)
import           Data.Functor                 ((<$>))
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SVM
import           GHC.Float

import           Control.Monad.ST.Safe        (runST, stToIO)
import           Foreign                      hiding (unsafeForeignPtrToPtr,
                                               unsafePerformIO)
import           Foreign.C.Types
import           Foreign.ForeignPtr.Unsafe    (unsafeForeignPtrToPtr)
import           System.IO.Unsafe             (unsafePerformIO)

type Position = SV.Vector CInt
type Velocity = SV.Vector CDouble

foreign import ccall "pso.h updateVelocity" c_updateVel::
  CInt->CInt->CDouble->CDouble->CDouble->Ptr CInt->Ptr CInt->Ptr CInt->Ptr CDouble->IO ()

getParameters::Double->(Double, Double, Double)
getParameters c = let w_max = 1.0
                      w_min = 0.4
                      c1 = 2
                      c2 = 2
                  in (w_max - c * (w_max - w_min), c1, c2)

foreignUpdateVel::Double->Int->
                  Position->Position->Position->Velocity->IO Velocity
foreignUpdateVel c m gbest pbest x v = do
  v' <- SV.unsafeThaw v
  let (w, c1, c2) = getParameters c
      (g_fptr, n) = SV.unsafeToForeignPtr0 gbest
      (p_fptr, _) = SV.unsafeToForeignPtr0 pbest
      (x_fptr, _) = SV.unsafeToForeignPtr0 x
  SVM.unsafeWith v' $
    c_updateVel (toEnum n) (toEnum m) (realToFrac w) (realToFrac c1) (realToFrac c2)
    (unsafeForeignPtrToPtr g_fptr)
    (unsafeForeignPtrToPtr p_fptr)
    (unsafeForeignPtrToPtr x_fptr)
  SV.unsafeFreeze v'

pos4i::Int->Velocity->Int->Int
pos4i m v i = SV.maxIndex $ SV.unsafeSlice (i*m) m v

nextPosition::Int->Int->Velocity->Position
nextPosition n m v =  SV.generate n $ fromIntegral . pos4i m v

data Particle = Particle { pos    :: Position
                         , vel    :: Velocity
                         , _order :: Orders}

instance NFData Particle where
  rnf (Particle a b c) = rnf a `seq` rnf b `seq` rnf c

instance Chromosome Particle where
  repMode _ = (3, 1)

  crossover p c [Particle gP _ _, Particle pP _ _, Particle cP vel o] =
    let n = nTask p
        m = n * nType p
        vel' = unsafePerformIO $ foreignUpdateVel c m gP pP cP vel
    in  return $ [Particle (nextPosition n m vel') vel' o]

  encode p s = do let (o, str) = toPool p s
                      num = nTask p * nTask p * nType p
                  vel <- SV.replicateM num $ getRandomR (-1, 1)
                  return $ Particle (SV.map toEnum . SV.convert $ str) vel o

  decode _ i = fromPool (_order i) (SV.convert . SV.map fromIntegral $ pos i)
