module Data.Graph where

import Prelude

import Data.Traversable

import Data.Maybe (Maybe(Nothing,Just))
import Data.Tuple (Tuple(Tuple))
import Data.StrMap as StrMap
import Data.StrMap (StrMap)
import Data.Array as Array

newtype Vertex = Vertex String
newtype Edge = Edge String

derive instance eqVertex :: Eq Vertex
derive instance eqEdge :: Eq Edge

unVertex :: Vertex -> String
unVertex (Vertex v) = v

unEdge :: Edge -> String
unEdge (Edge e) = e

type Graph v e =
  { vertices :: StrMap v
  , edges :: StrMap e
  }

newtype GraphV e v = GraphV (Graph v e)

runGraphV :: forall v e. GraphV e v -> Graph v e
runGraphV (GraphV g) = g

instance functorGraphV :: Functor (GraphV e) where
  map f (GraphV g) = GraphV (g { vertices = f <$> g.vertices }) 

instance foldableGraphV :: Foldable (GraphV e) where
  foldl f x (GraphV g) = foldl f x g.vertices
  foldr f x (GraphV g) = foldr f x g.vertices
  foldMap f (GraphV g) = foldMap f g.vertices

instance traversableGraphV :: Traversable (GraphV e) where
  traverse f (GraphV g) = GraphV <$> g { vertices = _ } <$> traverse f g.vertices
  sequence (GraphV g) = GraphV <$> g { vertices = _ } <$> sequence g.vertices
  
newtype GraphE v e = GraphE (Graph v e)

runGraphE :: forall v e. GraphE v e -> Graph v e
runGraphE (GraphE g) = g

instance functorGraphE :: Functor (GraphE e) where
  map f (GraphE g) = GraphE (g { edges = f <$> g.edges }) 

instance foldableGraphE :: Foldable (GraphE e) where
  foldl f x (GraphE g) = foldl f x g.edges
  foldr f x (GraphE g) = foldr f x g.edges
  foldMap f (GraphE g) = foldMap f g.edges

instance traversableGraphE :: Traversable (GraphE e) where
  traverse f (GraphE g) = GraphE <$> g { edges = _ } <$> traverse f g.edges
  sequence (GraphE g) = GraphE <$> g { edges = _ } <$> sequence g.edges

mapG :: forall v v' e e'. (v -> v') -> (e -> e') -> Graph v e -> Graph v' e'
mapG fv fe g = { vertices: fv <$> g.vertices, edges: fe <$> g.edges }

mapV :: forall v v' e. (v -> v') -> Graph v e -> Graph v' e
mapV fv g = { vertices: fv <$> g.vertices, edges: g.edges }

mapE :: forall v e e'. (e -> e') -> Graph v e -> Graph v e'
mapE fe g = { vertices: g.vertices, edges: fe <$> g.edges }

type VIn v = { id :: String | v }
type V v = { id :: Vertex | v }
type EIn e = { id :: String, source :: String, target :: String | e }
type E e = { id :: Edge, source :: Vertex, target :: Vertex | e }

emptyGraph :: forall v e. Graph (V v) (E e)
emptyGraph = {vertices: StrMap.empty, edges: StrMap.empty}

mkGraph :: forall v e. 
  { vertices :: Array (VIn v)
  , edges :: Array (EIn e)} ->
  Graph (V v) (E e)
mkGraph {vertices, edges} = {vertices: vertices', edges: edges'}
  where
  vertices' = vertices <#> vertexTuple # StrMap.fromFoldable
  vertexTuple v =
    Tuple v.id $ updateVertex v
  updateVertex v = v{id = Vertex v.id}
  edges' = edges <#> edgeTuple # StrMap.fromFoldable
  edgeTuple e = 
    Tuple e.id $ updateEdge e
  updateEdge e = e
    { id = Edge e.id
    , source = Vertex e.source
    , target = Vertex e.target
    }

vertexArray :: forall v e. Graph v e -> Array v
vertexArray g = Array.fromFoldable $ StrMap.values g.vertices

edgeArray :: forall v e. Graph v e -> Array e
edgeArray g = Array.fromFoldable $ StrMap.values g.edges

lookupV :: forall v e. Vertex -> Graph v e -> Maybe v
lookupV v g = StrMap.lookup (unVertex v) g.vertices 

lookupE :: forall v e. Edge -> Graph v e -> Maybe e
lookupE e g = StrMap.lookup (unEdge e) g.edges

incidentEdges :: forall v e. Vertex -> Graph (V v) (E e) -> Array {id :: Edge, source :: V v, target :: V v | e}
incidentEdges v g = edgeArray g # (Array.mapMaybe $ \e ->
  if e.source /= v && e.target /= v
  then Nothing
  else do
    source' <- lookupV e.source g
    target' <- lookupV e.target g
    pure e{source = source', target=target'})

edgeContext :: forall v e. Edge -> Graph v { source :: Vertex, target :: Vertex | e} -> Maybe { source :: v, target :: v | e}
edgeContext e g = do
    eProps <- lookupE e g
    source <- lookupV eProps.source g
    target <- lookupV eProps.target g
    pure eProps{ source = source, target = target }

source :: forall v e. Edge -> Graph v {source :: Vertex | e} -> Maybe v
source e g = do 
  props <- lookupE e g
  lookupV props.source g 

target :: forall v e. Edge -> Graph v {target :: Vertex | e} -> Maybe v
target e g = do 
  props <- lookupE e g
  lookupV props.target g

mergeVertices :: forall v e. 
  (Maybe v -> Maybe v -> Maybe v) -> Graph v e -> Graph v e -> Graph v e
mergeVertices f g1 g2 = g1{vertices=vertices}
  where
  vertices = g1.vertices `StrMap.union` g2.vertices
    # StrMap.keys 
    <#> (\k -> Tuple k <$> f (StrMap.lookup k g1.vertices) (StrMap.lookup k g2.vertices)) 
    # Array.catMaybes 
    # StrMap.fromFoldable