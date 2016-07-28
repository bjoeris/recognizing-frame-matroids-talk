module Data.Graph.Force where

import Prelude
import Data.Graph
import Data.Tuple (Tuple(Tuple))
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Monoid

type PV v = { x :: Number, y :: Number, vx :: Number, vy :: Number | v }

foreign import data Force :: # * -> * -- = Graph (PV v) e -> Graph (PV v) e

foreign import moveNodes :: forall v. Force v

foreign import dampVelocity :: forall v. Number -> Force v

foreign import iterateForce :: forall v. Int -> Force v -> Force v

foreign import applyForceNodes :: forall v. Force v -> Array (PV (id :: Vertex | v)) -> Array (PV (id :: Vertex | v))

foreign import appendForce :: forall v. Force v -> Force v -> Force v

foreign import emptyForce :: forall v. Force v

foreign import pull :: forall v. Number -> Number -> Force v

foreign import fixPositions :: forall v. StrMap { x :: Number, y :: Number } -> Force v

instance semigroupForce :: Semigroup (Force v) where
  append = appendForce

instance monoidForce :: Monoid (Force v) where
  mempty = emptyForce

applyForce :: forall v e. Force v -> Graph (PV (id :: Vertex | v)) e -> Graph (PV (id :: Vertex | v)) e
applyForce f g = g
  { vertices = applyForceNodes f (vertexArray g)
      <#> (\v -> Tuple (unVertex v.id) v)
      # StrMap.fromFoldable
  }
