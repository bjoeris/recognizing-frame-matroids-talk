module Data.Graph.Force where

import Prelude
import Data.Graph

type PV v = { x :: Number, y :: Number, vx :: Number, vy :: Number | v }

type Force v e = Graph (PV v) e -> Graph (PV v) e

applyForce :: forall v e. Force v e -> Graph (PV v) e -> Graph (PV v) e
applyForce f g = mapV move $ f g
  where
  move v = v{ x = v.x + v.vx, y = v.y + v.vy }