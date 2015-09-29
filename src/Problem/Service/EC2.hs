{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Problem.Service.EC2 (EC2, mkService) where

import           Data.Vector                ((!), (//))
import qualified Data.Vector                as Vec
import           Problem

import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor
import           GHC.Generics               (Generic)

-- Definition --

data EC2 = EC2 { ecus       :: Vec.Vector CU
               , prices     :: Vec.Vector Cost
               , bandwidths :: Vec.Vector Bandwidth
               , _cachedBW  :: Vec.Vector Bandwidth
               , insLimit   :: Int
               , _n         :: Int}

instance Service EC2 where
  s_cu s = Vec.unsafeIndex (ecus s)
  s_bw s t0 t1 = Vec.unsafeIndex (_cachedBW s) (t0 * _n s + t1)
  s_insPrice s = Vec.unsafeIndex (prices s)
  s_nType = Vec.length . ecus
  s_maxIns = insLimit
  s_qcharge s (p, ts, te) = (Vec.unsafeIndex (prices s) p) *  fromIntegral hs
    where hs = ceiling . (/3600) $ te - ts ::Int

  --s_qcharge s (p, ts, te) = (prices s ! p) / 60 * fromIntegral hs
  --  where hs = ceiling . (/60) $ te - ts :: Int
-- Creation --

data InsItem = InsItem { name      :: String
                       , price     :: Cost
                       , ecu       :: CU
                       , bandwidth :: Bandwidth} deriving Generic

instance FromJSON InsItem

baseECU::Double
baseECU = 8

jsonFile::String
jsonFile = "./resources/services/EC2/info.json"

readInfo::String->IO (Maybe [InsItem])
readInfo f = decode <$> BL.readFile f

cacheBW::Vec.Vector Bandwidth->Vec.Vector Bandwidth
cacheBW bs = foldr f e $ [(x,y)|x<-[0..n-1], y<-[0..n-1]]
  where n = Vec.length bs
        e = Vec.replicate (n*n) 0
        f (x, y) s = s // [(x*n+y, min (bs ! x) (bs ! y))]

mkService::Int->IO EC2
mkService limit = do Just is <- readInfo jsonFile
                     let bws = Vec.fromList . map bandwidth $ is
                     return $
                       EC2 { ecus = Vec.fromList . map ((/baseECU) . ecu) $ is
                           , prices = Vec.fromList . map price $ is
                           , bandwidths = bws
                           , _cachedBW = cacheBW bws
                           , insLimit = limit
                           , _n = length is}
