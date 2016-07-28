module Data.Graph.Force.ManyBody where

import Prelude

import Data.StrMap as StrMap
import Data.Tuple (Tuple(Tuple))
import Data.Graph.Force
import Data.Graph

type ManyBodyOptions v =
  { strength :: PV v -> Number
  }

manyBodyOptions :: forall v. ManyBodyOptions v
manyBodyOptions = { strength: \_ -> -30.0 }

foreign import manyBody :: forall v. ManyBodyOptions v -> Force v

-- manyBody :: forall v e. ManyBodyOptions (id :: Vertex | v) -> Int -> Force (id :: Vertex | v) e
-- manyBody opt numIterations g = g
--   { vertices = vertexArray g 
--       # manyBodyNodes opt numIterations
--       <#> (\v -> Tuple (unVertex v.id) v)
--       # StrMap.fromFoldable
--   }