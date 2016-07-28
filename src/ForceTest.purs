module ForceTest where

import Prelude hiding (id)

import Data.Maybe(Maybe(Just,Nothing), fromMaybe)
import Data.Array as Array
import Data.Time.Duration (Milliseconds, Seconds(Seconds))
import Data.Int (floor, toNumber)

import Halogen.HTML (HTML())
import Halogen.CSS.Units (rem)
import Halogen.SVG.Elements.Indexed as SVG
import Halogen.SVG.Properties.Indexed as SVG
import Halogen.SVG.Elements.Indexed
import Halogen.SVG.Elements.Indexed as E
import Halogen.SVG.Properties.Indexed
import Halogen.SVG.Properties.Indexed as P
import Halogen.HTML.Styles.Indexed
import Halogen.HTML.Properties.Tweened (tweenProp)
-- import Halogen.SVG.Elements.Tweened as SVG
-- import Halogen.SVG.Properties.Tweened as SVG
-- import Halogen.SVG.Elements.Tweened
-- import Halogen.SVG.Elements.Tweened as E
-- import Halogen.SVG.Properties.Tweened
-- import Halogen.SVG.Properties.Tweened as P
-- import Halogen.HTML.Styles.Tweened
-- import Halogen.SVG.Elements.Indexed as I
-- import Halogen.SVG.Properties.Indexed as I
-- import Halogen.SVG.Elements.Indexed as I
-- import Halogen.SVG.Properties.Indexed as I
-- import Halogen.HTML.Styles.Indexed as I
import Halogen.SVG.Properties (round,Align(Mid),userSpaceOnUse)

import Timeline.Build as Timeline
import Timeline.Tween as Timeline
import Timeline.Compile as Timeline
import Timeline.Run as Timeline
import Timeline.Tween (TTween, tween, Tween)

import D3.Geo
import D3.Geo as D3

import Data.Graph.Force.ManyBody
import Data.Graph.Force.Link
import Data.Graph.Force
import Data.Graph.Render.SVG as Graph 
import Data.Graph as Graph

import Math (pi)

forceTest :: forall b p i. TTween b (HTML p i)
forceTest = 
  forceTest' <$> Timeline.newStep' Timeline.StepEnd (Seconds 5.0)

  where
  size = 50.0
  padding = 
    { left: 2.0
    , right: 2.0
    , top: 2.0
    , bottom: 8.0
    }
  boundingBox =
    { left: (-size/2.0 + padding.left)
    , right: size/2.0 - padding.right
    , top: (-size/2.0 + padding.top)
    , bottom: size/2.0 - padding.bottom
    }
  viewBox' = viewBox (-size/2.0) (-size/2.0) size size

  iterations = 1000

  -- g :: Graph (Graph.PV (id :: Graph.Vertex)) (Graph.PE ())
  g0 = Graph.mkRGraph
    { vertices :
      [ { id: "1", x: 0.33, y: 0.77 } 
      , { id: "2", x: 0.72, y: 0.76 }
      , { id: "3", x: 0.87, y: 0.40 }
      , { id: "4", x: 0.62, y: 0.51 }
      , { id: "5", x: 0.35, y: 0.40 }
      , { id: "6", x: 0.23, y: 0.14 }
      , { id: "7", x: 0.09, y: 0.51 }
      ]
    , edges : 
      [ { id: "a", source: "1", target: "2" } 
      , { id: "b", source: "2", target: "3" } 
      , { id: "c", source: "3", target: "4" } 
      , { id: "d", source: "4", target: "5" } 
      , { id: "e", source: "5", target: "6" } 
      , { id: "f", source: "6", target: "7" } 
      , { id: "g", source: "7", target: "1" } 
      , { id: "h", source: "2", target: "4" }
      , { id: "i", source: "5", target: "7" }
      , { id: "j", source: "1", target: "1" }
      , { id: "k", source: "2", target: "2" }
      ] 
    } 
    # Graph.autoTransform boundingBox
    # Graph.mapV (\v -> { id: v.id, x: v.x, y: v.y, radius: v.radius, classes: v.classes, vx: 0.0, vy: 0.0 })
  g' t = 
    let i = floor ((toNumber iterations) * t / 1.0)
        f = manyBody manyBodyOptions 
            <> link linkOptions (Graph.edgeArray g0)
            <> dampVelocity 0.1
            <> moveNodes
    in g0 # applyForce (iterateForce i f)
        # Graph.autoTransform boundingBox
  g1 = g' 1.0
  g = Graph.interpolateGraphSeq 
    [ {time: 0.0, value: g0}
    , {time: 1.0, value: g1}
    ]
    -- if t <= 0.0
    -- then g0
    -- else if t >= 1.0
    --      then g1
    --      else g' t

  forceTest' step = do
    t <- tween step (\t'->t')
    pure $ svg [SVG.width (size # rem), SVG.height (size # rem), viewBox']
      [ Graph.render (g t) ]