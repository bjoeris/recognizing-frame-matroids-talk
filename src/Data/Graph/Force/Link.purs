module Data.Graph.Force.Link where

import Prelude

import Data.StrMap as StrMap
import Data.Tuple (Tuple(Tuple))
import Data.Graph.Force
import Data.Graph

type LinkOptions v e =
  { distance :: e -> PV v -> PV v -> Number
  }

linkOptions :: forall v e. LinkOptions v e
linkOptions = { distance: \_ _ _ -> 30.0 }

foreign import link :: forall v e. LinkOptions v e -> Array e -> Force v

-- link :: forall v e. LinkOptions (id :: Vertex | v) e -> Int -> Force (id :: Vertex | v) e
-- link opt numIterations g = g
--   { vertices = vertices' 
--       <#> (\v -> Tuple (unVertex v.id) v)
--       # StrMap.fromFoldable
--   }
--   where 
--   vertices' = linkNodes opt numIterations (vertexArray g) (edgeArray g)