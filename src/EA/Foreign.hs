{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes               #-}

module EA.Foreign (spea2Select) where

import           Data.Functor          ((<$>))
import qualified Data.Vector           as Vec
import qualified Data.Vector.Storable  as VS
import           Foreign               hiding (unsafePerformIO)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free)
import           Foreign.Marshal.Array (mallocArray, newArray, peekArray)
import           System.IO.Unsafe      (unsafePerformIO)

import           EA                    (EnvSelector)
import           MOP                   (WithObjs (..), (@!))

type CEnvSelector = Ptr CDouble->Ptr CDouble->CSize->CSize->Ptr CSize->IO ()

foreign import ccall "spea2.h spea2_select_2d" c_spea2Select::CEnvSelector

cSelectorWrapper::CEnvSelector->EnvSelector
cSelectorWrapper cSel p0 p1 = unsafePerformIO $ do
  let p = p0 Vec.++ p1
      os = Vec.toList $ Vec.map getObjs p
  sp <- mallocArray (Vec.length p0)
  xs <- newArray $ map (realToFrac . (@!0)) os
  ys <- newArray $ map (realToFrac . (@!1)) os

  cSel xs ys (toEnum $ Vec.length p) (toEnum $ Vec.length p0) sp
  sel <- peekArray (Vec.length p0) sp

  free sp
  free ys
  free xs
  return . Vec.map ((p Vec.!) . fromIntegral) . Vec.fromList $ sel

spea2Select::EnvSelector
spea2Select = cSelectorWrapper c_spea2Select
