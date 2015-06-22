{-#LANGUAGE ForeignFunctionInterface#-}

module EA.Chromosome.Foreign (probSelect) where

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as VS
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Storable ((!))

foreign import ccall "chromosome.h prob_select"
  c_probSelect::CDouble->CSize->Ptr CInt->IO ()

probSelect::Double->Vec.Vector a->(Vec.Vector a, Vec.Vector a)
probSelect p vs = unsafePerformIO $ do
  let d = VS.replicate (Vec.length vs) 0
  VS.unsafeWith d (c_probSelect (realToFrac p) (toEnum $ Vec.length vs))
  let v0 = Vec.ifilter (\i _->(d!i) == 0) vs
  let v1 = Vec.ifilter (\i _->(d!i) /= 0) vs
  return (v0, v1)
