module Problem.DAG.Convertor.Pegasus (parseFile) where

import           Data.Function        (on)
import qualified Data.IntMap          as IntMap
import           Data.List            (groupBy)
import qualified Data.Map             as Map
import qualified Data.Vector          as Vec
import           Text.HandsomeSoup    (css)
import           Text.XML.HXT.Core
import           Text.XML.HXT.TagSoup

import           Problem.DAG.TinyDAG  (TinyDAG)

import           Debug.Trace          (traceShow)

type RawNode = (String, [(String, (String, String))])
type FileSources = Map.Map String [Int]
type PredInfo = [IntMap.IntMap Double]

readNodes = css "job" >>> (getAttrValue "id" &&&
                           getAttrValue "runtime" &&&
                           (css "uses" >>> (getAttrValue "file" &&&
                                            getAttrValue "link" &&&
                                            getAttrValue "size")))

readControl = css "child" >>>
              ((css "parent" >>> getAttrValue "ref") &&& getAttrValue "ref")

parseFile::String->IO TinyDAG
parseFile path = do
  let doc = readDocument [withTagSoup, withValidate no] path
  rc <- runX $ readNodes <<< doc
  ct <- runX $ readControl <<< doc
  let gs = groupBy ((==) `on` fst) rc
      ns = map ((\xs->(fst $ head xs, map snd xs)). map snd) gs
      ds = Map.fromList $ zip (map (fst . head) gs) [0..]
      cs = map (\(x, y)->((Map.!) ds x, (Map.!) ds y)) ct
      rs = map (read . fst) ns
      pi = parsePreds ns cs
  return . Vec.fromList $ zip rs pi

findSources::[RawNode]->FileSources
findSources ns =
  let fs = Map.empty
      _insert i (_, ("input", _)) f = f
      _insert i (k, ("output", _)) f = let s = Map.findWithDefault [] k f
                                       in Map.insert k (i:s) f
      _insPN (i, (_, fs)) f = foldr  (_insert i) f fs
  in foldr _insPN fs $ zip [0..] ns

parsePreds::[RawNode]->[(Int, Int)]->PredInfo
parsePreds ns rs =
  let _fs = findSources ns
      _pi = Vec.replicate (length ns) IntMap.empty
      _insert i (fn, ("input", s)) ps =
        if (Map.member fn _fs) then
          let ti = (Vec.!) ps i
              pds = filter (\x->IntMap.member x ti) $ (Map.!) _fs fn
              -- ss = [IntMap.findWithDefault 0 p ti+read s|p<-pds]
              ss = [(IntMap.!) ti p + read s|p<-pds]
              tu = IntMap.fromList $ zip pds ss
          in (Vec.//) ps [(i, IntMap.union tu ti)]
        else ps
      _insert _ (_, ("output", _)) ps = ps
      _insPN (i, (_, fs)) ps = foldr (_insert i) ps fs
      _insCtrl (i, j) ps = let t = (Vec.!) ps j
                           in (Vec.//) ps [(j, IntMap.insert i 0 t)]
      pi' = foldr _insCtrl _pi rs
      pi'' = foldr _insPN pi' $ zip [0..] ns
  in Vec.toList pi''
