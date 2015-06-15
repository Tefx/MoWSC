{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes               #-}

module EA.Foreign (spea2Select) where

import qualified Data.Vector           as Vec
import qualified Data.Vector.Storable  as VS
import           Foreign
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free)
import           Foreign.Marshal.Array (newArray)
import           System.IO.Unsafe      (unsafePerformIO)

import           EA                    (EnvSelector)
import           MOP                   (WithObjs (..), (@!))

type CEnvSelector = Ptr CDouble->Ptr CDouble->CSize->CSize->Ptr CInt->IO ()

foreign import ccall "spea2.h spea2_select_2d" c_spea2Select::CEnvSelector

cSelectorWrapper::CEnvSelector->EnvSelector
cSelectorWrapper cSel p0 p1 = unsafePerformIO $ do
  let p = p0 Vec.++ p1
      os = Vec.toList $ Vec.map getObjs p
      sel = VS.replicate (Vec.length p0) 0
  xs <- newArray $ map (realToFrac . (@!0)) os
  ys <- newArray $ map (realToFrac . (@!1)) os

  VS.unsafeWith sel $
    cSel xs ys (toEnum $ Vec.length p) (toEnum $ Vec.length p0)

  free ys
  free xs
  return . Vec.map ((p Vec.!) . fromIntegral) $ VS.convert sel

spea2Select::EnvSelector
spea2Select = cSelectorWrapper c_spea2Select
