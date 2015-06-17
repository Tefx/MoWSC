{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Problem.DAG.TinyDAG (TinyDAG, encodeDAG, fromFile) where

import           Data.Aeson                 (decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Functor               ((<$>))
import qualified Data.IntMap                as IntMap
import           Data.Maybe                 (fromJust)
import qualified Data.Vector                as Vec

import           Problem                    (Data, Time, Workflow (..))

type TinyNode = (Time, IntMap.IntMap Data)
type TinyDAG = Vec.Vector TinyNode

instance Workflow TinyDAG where
  w_preds w = map fst . IntMap.toList . snd . (Vec.!) w
  w_hasEdge w ti tj = IntMap.member ti . snd $ (Vec.!) w tj

  w_comm w ti tj = flip (IntMap.!) ti . snd $ (Vec.!) w tj
  w_refTime w = fst . (Vec.!) w
  w_nTask = Vec.length

encodeDAG::TinyDAG->BL.ByteString
encodeDAG = encode . topSort . addPesudeNode

fromFile::String->IO TinyDAG
fromFile path = fromJust . decode <$> BL.readFile path

addPesudeNode::TinyDAG->TinyDAG
addPesudeNode d = let _addEntry (t, pd)
                        | IntMap.null pd = (t, IntMap.singleton (Vec.length d) 0)
                        | otherwise = (t, pd)
                      exit = (0,) . IntMap.fromList . map (,0) $
                             filter (null . w_succs d) [0..w_nTask d-1]
                   in (flip Vec.snoc) exit . (flip Vec.snoc) (0, IntMap.empty) $
                      Vec.map _addEntry d

popFirst::Vec.Vector (Int,TinyNode)->(Vec.Vector Int, Vec.Vector (Int,TinyNode))
popFirst d = let (h,r) = Vec.partition (IntMap.null . snd . snd) d
                 fs = Vec.map fst h
                 _rmf (i, (t, pd)) = (i, (t, Vec.foldr (IntMap.delete) pd fs))
                 d' = Vec.map _rmf r
             in (fs, d')

getOrder::Vec.Vector (Int,TinyNode)->Vec.Vector Int->Vec.Vector Int
getOrder d o
  | Vec.null d = o
  | otherwise = let (hs, d') = popFirst d
                in getOrder d' ((Vec.++) o hs)

topSort::TinyDAG->TinyDAG
topSort d = let o = getOrder (Vec.imap (,) d) $ Vec.empty
                loc = (IntMap.!) . IntMap.fromList . Vec.toList $ Vec.imap (flip (,)) o
                _rpMap = IntMap.fromList . map (\(x, y)->(loc x, y)) . IntMap.toList
                _newNode i = let (t, pd) = (Vec.!) d $ (Vec.!) o i
                             in (t, _rpMap pd)
            in Vec.generate (Vec.length d) _newNode
