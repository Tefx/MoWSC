{-# LANGUAGE RankNTypes #-}

module EA.Selection ( rouletteSelGen
                    , tournamentSelGen
                    , nullSelGen) where

import           EA                   (MutSelector)
import           EA.Utils             (FitnessAssigner, sortByFit)
import           MOP                  (Objectives (..), WithObjs (..))
import           Utils                (With (..))
import           Utils.Random         (uniRandPos)

import           Control.Monad        (replicateM)
import           Control.Monad.Random (RandomGen, getRandomR)
import           Data.Vector          ((!))
import qualified Data.Vector          as Vec

rouletteSelGen::(Ord f)=>FitnessAssigner f->MutSelector
rouletteSelGen fa pop n =
  let l = map ((1/) . fromIntegral) [1..Vec.length pop] :: [Double]
      ws = Vec.fromList $ map (/sum l) l
      max = Vec.maximum ws
      pop' = sortByFit fa pop
      select = do i <- getRandomR (0, Vec.length ws-1)
                  p <- getRandomR (0, 1)
                  if p < (ws!i) / max
                    then return $ pop ! i
                    else select
  in replicateM n select

tournamentSelGen::MutSelector
tournamentSelGen pop n = replicateM n $
                         do [l0, l1] <- uniRandPos 2 $ Vec.length pop
                            let i0 = pop ! l0
                                i1 = pop ! l1
                            return $ if getObjs i0 <<< getObjs i1 then i0 else i1

nullSelGen::MutSelector
nullSelGen pop n = return . take n $ Vec.toList pop
