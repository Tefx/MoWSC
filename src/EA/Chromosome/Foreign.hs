{-# LANGUAGE ForeignFunctionInterface #-}

module EA.Chromosome.Foreign (probSelect, olTrans) where

import           Data.Functor          ((<$>))
import qualified Data.Vector           as Vec
import           Data.Vector.Storable  ((!))
import qualified Data.Vector.Storable  as VS
import           Foreign               hiding (unsafePerformIO)
import           Foreign.C.Types
import           Foreign.Marshal.Array (mallocArray, peekArray)
import           System.IO.Unsafe      (unsafePerformIO)

foreign import ccall "chromosome.h prob_select"
  c_probSelect::CDouble->CSize->Ptr CInt->IO ()
foreign import ccall "chromosome.h ol_trans"
  c_olTrans::CInt->Ptr CInt->Ptr CInt->IO ()

probSelect::Double->Vec.Vector a->(Vec.Vector a, Vec.Vector a)
probSelect p vs = unsafePerformIO $ do
  let d = VS.replicate (Vec.length vs) 0
  VS.unsafeWith d (c_probSelect (realToFrac p) (toEnum $ Vec.length vs))
  let v0 = Vec.ifilter (\i _->(d!i) /= 0) vs
  let v1 = Vec.ifilter (\i _->(d!i) == 0) vs
  return (v0, v1)

olTrans::Vec.Vector Int->[Int]
olTrans xs = unsafePerformIO $ do
  ys <- mallocArray (Vec.length xs)
  VS.unsafeWith (VS.map toEnum $ VS.convert xs) (c_olTrans (toEnum $ Vec.length xs) ys)
  res <- map fromIntegral <$> peekArray (Vec.length xs) ys
  free(ys)
  return res
