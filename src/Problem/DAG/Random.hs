{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Problem.DAG.Random (mkWorkflow) where

import           Data.IntMap                (IntMap, findWithDefault)
import qualified Data.IntSet                as ISet
import           Data.Vector                (Vector, (!))
import           Problem

import           Data.Aeson                 (FromJSON, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           GHC.Generics               (Generic)

import           Control.Applicative        (liftA)
import           Data.Maybe                 (fromJust)


data RWF = RWF { j_preds :: Vector ISet.IntSet
               , j_succs :: Vector ISet.IntSet
               , j_reft  :: Vector Time
               , j_num   :: Int
               , j_data  :: IntMap Data
               } deriving Generic

instance FromJSON RWF
instance Workflow RWF where

  w_preds w = ISet.toList . (j_preds w !)
  w_succs w = ISet.toList . (j_succs w !)

  w_inp w t = ISet.member t . (j_preds w !)
  w_ins w t = ISet.member t . (j_succs w !)

  w_nTask = j_num

  w_comm w t0 t1 = findWithDefault 0 (t0 * (j_num w) + t1) $ j_data w
  w_refTime w = (j_reft w !)

mkWorkflow::String->IO RWF
mkWorkflow = liftA (fromJust . decode) . BL.readFile
