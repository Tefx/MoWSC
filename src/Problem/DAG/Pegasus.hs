module Problem.DAG.Pegasus (Pegasus, mkWorkflow) where

import           Problem

import           Data.Function
import           Data.List
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import           Data.Vector          ((!))
import qualified Data.Vector          as Vec
import           Text.HandsomeSoup    (css)
import           Text.XML.HXT.Core
import           Text.XML.HXT.TagSoup

-- Definition --

data Pegasus = Pegasus { _preds :: Vec.Vector (Set.Set Task)
                       , _succs :: Vec.Vector (Set.Set Task)
                       , _datas :: Map.Map Int Data
                       , _time  :: Vec.Vector Time
                       , _num   :: Int}

instance Workflow Pegasus where

  w_preds w = Set.toList . (_preds w !)
  w_succs w = Set.toList . (_succs w !)

  w_hasEdge w ti tj = Set.member ti $ _preds w ! tj

  w_nTask = _num

  w_comm w t0 t1 = Map.findWithDefault 0 (t0 * (_num w) + t1) $ _datas w
  w_refTime w = (_time w !)


-- Creation --

type TaskID = String
type RawRuntime = [(TaskID, String)]
type RawUses = [(TaskID, (String, (String, String)))]
type RawControl = [(TaskID,TaskID)]

readRuntime = css "job" >>>
              (getAttrValue "id" &&& getAttrValue "runtime")

readUses = css "job" >>>
           (getAttrValue "id" &&& (css "uses" >>>
                                   (getAttrValue "file" &&&
                                    getAttrValue "link" &&&
                                    getAttrValue "size")))

readControl = css "child" >>>
              ((css "parent" >>> getAttrValue "ref") &&& getAttrValue "ref")

groupByLoc:: (Ord b, Eq b)=>(a->b)->[a]->[[a]]
groupByLoc f = groupBy ((==) `on` f) .
               sortBy (compare `on` f)

getRuntime::[(TaskID, String)] -> [(TaskID, Time)]
getRuntime = map (\x -> case x of
                         (t, s) -> (t, read s::Double))

getFromTo::Int -> [(String, (Int, (TaskID, Data)))] -> [TaskID]
getFromTo tag = (\x -> if null x then ["_"] else x) .
                map (fst . snd) .
                filter ((== tag) . fst) .
                map snd

getSize::[(String, (Int, (TaskID, Data)))] -> Data
getSize = snd. snd. snd . head

getItems::[(String, (Int, (TaskID, Data)))] -> [((TaskID, TaskID), Data)]
getItems x = let froms = head $ getFromTo 0 x
                 tos = getFromTo 1 x
                 size = getSize x
             in zip (zip (repeat froms) tos) (repeat size)

sumData::[(a, Data)] -> (a, Data)
sumData x@((dep,_):_) = (dep, sum $ map snd x)

getData::[(TaskID, (String, (String, String)))] -> [((TaskID, TaskID), Data)]
getData = concat .
          map getItems .
          groupByLoc fst .
          map (\x -> case x of
                      (t, (f, ("input", s))) -> (f, (1, (t, read s::Double)))
                      (t, (f, ("output", s))) -> (f, (0, (t, read s::Double))))

addPDeps::[((TaskID, TaskID), Data)]->
          [(TaskID, Time)]->[((TaskID, TaskID),Data)]
addPDeps datas runtime = let ts = map fst runtime
                   in [(("_", x),0)|x<-ts] ++ [((x, "_"),0)|x<-ts] ++ datas

mergeDeps::(Eq a, Ord a)=>[a]->[(a, Data)]->[(a, Data)]
mergeDeps c d = let deps = d ++ map (\x -> (x, 0)) c
                in map sumData $ groupByLoc fst deps

replaceDeps::[((TaskID, TaskID), Data)] -> [((TaskID, TaskID), Data)]
replaceDeps = map (\x -> case x of
                        (("_", a), _) -> (("entry", a), 0)
                        ((a, "_"), _) -> ((a, "exit"), 0)
                        x -> x)

addPN::[(TaskID, Time)] -> [(TaskID, Time)]
addPN = ([("entry", 0), ("exit", 0)] ++)

readXML::String->IO ([((TaskID, TaskID), Data)],[(TaskID, Time)])
readXML path = do
  let doc = readDocument [withTagSoup, withValidate no] path
  runtime <- runX $ doc >>> readRuntime
  uses <- runX $ doc >>> readUses
  control <- runX $ doc >>> readControl
  return $ parse runtime uses control

parse::RawRuntime->RawUses->RawControl->([((TaskID, TaskID), Data)],[(TaskID, Time)])
parse runtime uses control =
  (replaceDeps $ mergeDeps control $ addPDeps datas rawRuntime,
   addPN rawRuntime)
  where rawRuntime = getRuntime runtime
        datas = getData uses

mkSuccs::(Ord a, Eq a)=>[a] -> [(a, a)] -> [(a, (Set.Set a))]
mkSuccs emps = (++ others) .
               map (\x->((fst $ head x), (Set.fromList $ map snd x))) .
               groupByLoc fst
  where others = [(x, Set.empty)|x<-emps]

mkPreds::(Ord a, Eq a)=>[a]->[(a, a)] -> [(a, (Set.Set a))]
mkPreds emps  = (++ others) .
                map (\x->((snd $ head x), (Set.fromList $ map fst x))) .
                groupByLoc snd
  where others = [(x, Set.empty)|x<-emps]

findRemove::(a->Bool)->[a]->(Maybe a, [a])
findRemove _ [] = (Nothing, [])
findRemove f (y:ys) | f y = (Just y, ys)
                    | otherwise = case findRemove f ys of
                                   (Nothing, ys') -> (Nothing, y:ys')
                                   (Just y', ys') -> (Just y', y:ys')

removeInAll::(Ord a)=>a->[(a, Set.Set a)]->[(a, Set.Set a)]
removeInAll x = map f
                where f (y, z) = (y, Set.delete x z)

topSort::(Ord a)=>[(a, (Set.Set a))]->[a]
topSort preds = case findRemove (Set.null . snd) preds of
                 (Just (x, _), preds') -> x:(topSort $ removeInAll x preds')
                 (Nothing, preds') -> []

indexTasks::[TaskID] -> [(TaskID, a)] -> [a]
indexTasks index = map snd .
                     sortBy (compare `on` fst).
                     map f
  where f (task, time) = case elemIndex task index of
                          (Just x) -> (x, time)

deepIndex::[TaskID] -> [(TaskID, Set.Set TaskID)] -> [Set.Set Task]
deepIndex index = map (Set.map f) . indexTasks index
  where f task = case elemIndex task index of
          (Just x) -> x

indexDeps::[TaskID] -> [((TaskID, TaskID), Data)] -> Map.Map Int Data
indexDeps index deps = Map.fromList $ map f2 deps
  where len = length index
        f task = case elemIndex task index of
          Just x -> x
        f2 ((t0, t1), size) = (((f t0)*len+(f t1)), size)

mkWorkflow::String->IO Pegasus
mkWorkflow fpath =
  do (deps, rawruntimes) <- readXML fpath
     let rawpreds = mkPreds ["entry"] $ map fst deps
         rawsuccs = mkSuccs ["exit"] $ map fst deps
         index = topSort rawpreds
     return $
       Pegasus { _preds = Vec.fromList $ deepIndex index rawpreds
               , _succs = Vec.fromList $ deepIndex index rawsuccs
               , _datas = indexDeps index deps
               , _time = Vec.fromList $ indexTasks index rawruntimes
               , _num = length index}
