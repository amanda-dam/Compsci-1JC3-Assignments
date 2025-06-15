{- Assignment 3
 - Name: Amanda Dam
 - Date: Sunday, June 6, 2021
 -}
module Assign_3 where
import Data.List

macid :: String
macid = "dama5"


{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
type Graph a = [(Node a,Edges)]
type Edges = [NodeID]
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph = [(nodeA,[1,2])
          ,(nodeB,[])
          ,(nodeC,[1,2])]

{- maxNodeID returns the largest NodeID in a given graph -}

maxNodeID :: Graph a -> Maybe NodeID
maxNodeID [] = Nothing
maxNodeID [x] = Just (getNodeID (fst x))
maxNodeID (x:x':xs)
  | Just (getNodeID (fst x)) >= Just (getNodeID (fst x')) = maxNodeID (x:xs)
  | otherwise = maxNodeID (x':xs)

{- insertNode inserts a new node with the given value into a given graph -}

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "No value"

insertNode :: a -> Graph a -> Graph a
insertNode v [] = [(Node 0 v, [])]
insertNode v graph = graph ++ [(Node (fromJust (maxNodeID graph) + 1) v, [])]

{- removeNode removes any Node with the given NodeID (including occurrences of NodeID in edges) from the given graph-}

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []  = []
removeItem x (y:ys) 
  | x == y    = removeItem x ys
  | otherwise = y : removeItem x ys

-- removes node
removeNode' :: NodeID -> Graph a -> Graph a
removeNode' nodeID [] = []
removeNode' nodeID (x:xs)
  | nodeID == getNodeID(fst x) = removeNode' nodeID xs
  | otherwise = x:removeNode' nodeID xs

-- removes NodeID in edges
removeFromEd :: NodeID -> (Node a,Edges) -> (Node a,Edges)
removeFromEd nID point
  | nID `elem` snd point = (fst point,removeItem nID (snd point))
  | otherwise = point

allEdges :: Graph a -> Edges
allEdges [] = []
allEdges (x:xs) = snd x ++ allEdges xs

removeNode ::  NodeID -> Graph a -> Graph a
removeNode nID [] = []
removeNode nID graph = removeNode' nID (map (\x -> if nID `elem` allEdges graph then removeFromEd nID x else x) graph)

{- lookupNode returns the node corresponding to the given NodeID in the given graph -}

lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID [] = Nothing
lookupNode nID (x:xs)
  | nID == getNodeID(fst x) = Just (Node (getNodeID(fst x)) (getNodeVal(fst x)))
  | otherwise = lookupNode nID xs

{- insertEdge inserts an edge from the Node with the first NodeID in the tuple to the Node with the second NodeID in the tuple -}

-- check if NodeID is aldready in edge 
edgeExists :: NodeID -> (Node a,Edges) -> Bool
edgeExists nID point
  | nID `elem` snd point = True
  | otherwise = False

-- check if node exists
nodeExists :: NodeID -> Graph a -> Bool
nodeExists nID [] = False
nodeExists nID (x:xs) 
  | nID == getNodeID(fst x) = True
  | otherwise = nodeExists nID xs

findPoint :: NodeID -> Graph a -> Maybe (Node a,Edges)
findPoint nID [] = Nothing
findPoint nID (x:xs)
  | nID == getNodeID(fst x) = Just x
  | otherwise = findPoint nID xs

addEdge :: (Node a,Edges) -> NodeID -> (Node a,Edges)
addEdge point nID = (fst point, snd point ++ [nID])

insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge (n1,n2) [] = Nothing
insertEdge (n1,n2) graph
  | nodeExists n1 graph && nodeExists n2 graph && not(edgeExists n2 (fromJust(findPoint n1 graph))) = Just(map (\x -> if getNodeID(fst x) == n1 then addEdge x n2 else x) graph)
  | nodeExists n1 graph && nodeExists n2 graph && edgeExists n2 (fromJust(findPoint n1 graph)) = Just graph
  | otherwise = Nothing