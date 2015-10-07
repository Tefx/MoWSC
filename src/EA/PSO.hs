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

import           Control.DeepSeq              (deepseq)
import           Control.Monad.ST.Safe        (runST, stToIO)
import           Foreign                      hiding (unsafeForeignPtrToPtr,
                                               unsafePerformIO)
import           Foreign.C.Types
import           Foreign.ForeignPtr.Unsafe    (unsafeForeignPtrToPtr)
import           System.IO.Unsafe             (unsafePerformIO)

import           Debug.Trace                  (traceShow)

type Position = SV.Vector CInt
type Velocity = SV.Vector CDouble

foreign import ccall "pso.h updateVelocity" c_updateVel::
  CInt->CInt->CDouble->CDouble->CDouble->Ptr CInt->Ptr CInt->Ptr CInt->Ptr CDouble->IO ()
foreign import ccall "pso.h randVelocity" c_randVel::CInt->Ptr CDouble->IO ()
foreign import ccall "pso.h updatePosition" c_updatePos::
  CInt->CInt->Ptr CDouble->Ptr CInt->IO ()

getParameters::Double->(CDouble, CDouble, CDouble)
getParameters c =
  let w_max = 1.0
      w_min = 0.4
      c1 = 2
      c2 = 2
  in (realToFrac $ w_max - c * (w_max - w_min), c1, c2)

foreignUpdateVel::Double->Int->
                  Position->Position->Position->Velocity->IO Velocity
foreignUpdateVel c m gbest pbest x v = do
  v' <- SV.unsafeThaw v
  let (w, c1, c2) = getParameters c
      (g_fptr, n) = SV.unsafeToForeignPtr0 gbest
      (p_fptr, _) = SV.unsafeToForeignPtr0 pbest
      (x_fptr, _) = SV.unsafeToForeignPtr0 x
  SVM.unsafeWith v' $
    c_updateVel (toEnum n) (toEnum m) w c1 c2
    (unsafeForeignPtrToPtr g_fptr)
    (unsafeForeignPtrToPtr p_fptr)
    (unsafeForeignPtrToPtr x_fptr)
  SV.unsafeFreeze v'

newPos::Int->Int->Velocity->IO Position
newPos n m v = do
  pos <- SVM.unsafeNew n
  let v_ptr = unsafeForeignPtrToPtr . fst $ SV.unsafeToForeignPtr0 v
  SVM.unsafeWith pos $ c_updatePos (toEnum n) (toEnum m) v_ptr
  SV.unsafeFreeze pos

randVel::Int->IO Velocity
randVel n = do
  v <- SVM.unsafeNew n
  SVM.unsafeWith v . c_randVel $ toEnum n
  SV.unsafeFreeze v

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
        pos' = unsafePerformIO $ newPos n m vel'
    in  return $ pos' `deepseq` vel' `deepseq` [Particle pos' vel' o]

  farewell i = i {chrm = (chrm i){vel=SV.empty}}

  encode p s = do let (o, str) = toPool p s
                      -- vel = unsafePerformIO . randVel $ nTask p * nTask p * nType p
                      vel = SV.replicate (nTask p * nTask p * nType p) 0
                  return $ Particle (SV.map toEnum . SV.convert $ str) vel o

  decode _ i = fromPool (_order i) (SV.convert . SV.map fromIntegral $ pos i)
