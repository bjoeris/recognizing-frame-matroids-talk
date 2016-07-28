module Data.Graph.Render.SVG where

import Prelude

import Halogen.SVG.Elements (SVG())
import Halogen.SVG.Elements.Indexed
import Halogen.SVG.Properties.Indexed
import Halogen.SVG.Elements.Indexed as SVG
import Halogen.SVG.Properties.Indexed as SVG

import Math (sin,cos)

import Halogen.CSS.Colors (Color, mkColor)
import Halogen.CSS.Units (rem)

import Data.Array as Array
import Data.Maybe (Maybe(..),maybe,fromMaybe)
import Data.String as String
import Data.Semigroup (class Semigroup)
import Data.Foldable (foldMap)

import Data.Graph
import Data.Graph as G

import Debug.Trace as Debug

type CircularLoopProps = { radius :: Number, angle :: Number }

data EdgeRenderType
  = LinearEdge
  | CircularLoop CircularLoopProps

type RE e = E ( classes :: Array String, renderType :: EdgeRenderType | e )
type RV e = V ( classes :: Array String, radius :: Number, x :: Number, y :: Number | e )
-- type RE = E ( color :: Color, width :: Number, renderType :: EdgeRenderType )
-- type RV = V ( x :: Number, y :: Number, radius :: Number, color :: Color, strokeColor :: Color, strokeWidth :: Number )

render' :: forall v e p i. (RV v -> SVG p i) -> (RE e -> RV v -> RV v -> SVG p i) -> Graph (RV v) (RE e) -> SVG p i
render' renderVertex renderEdge g = SVG.g [] (edges <> vertices)
  where
  vertices = g # G.vertexArray <#> renderVertex
  edges = g # G.edgeArray # Array.mapMaybe
    (\e -> renderEdge e <$> lookupV e.source g <*> lookupV e.target g)

defaultRenderProps :: Graph (V ( x :: Number, y :: Number)) (E ()) -> Graph (RV ()) (RE ())
defaultRenderProps = mapG vertexRenderProps edgeRenderProps
  where
  vertexRenderProps :: V (x :: Number, y :: Number) -> RV ()
  vertexRenderProps v = 
    { id: v.id
    , x: v.x
    , y: v.y
    , radius: 2.0
    , classes: ["vertex"]
    }
  edgeRenderProps :: E () -> RE ()
  edgeRenderProps e =
    { id: e.id
    , source: e.source
    , target: e.target
    , renderType: LinearEdge
    , classes: ["edge"]
    }
-- defaultRenderProps :: Graph (V ( x :: Number, y :: Number)) (E ()) -> Graph RV RE
-- defaultRenderProps = mapG vertexRenderProps edgeRenderProps
--   where
--   vertexRenderProps :: V (x :: Number, y :: Number) -> RV
--   vertexRenderProps v = 
--     { id: v.id
--     , x: v.x
--     , y: v.y
--     , radius: 0.05
--     , color: mkColor "black"
--     , strokeColor: mkColor "white"
--     , strokeWidth: 0.01
--     }
--   edgeRenderProps :: E () -> RE
--   edgeRenderProps e =
--     { id: e.id
--     , source: e.source
--     , target: e.target
--     , renderType: LinearEdge
--     , color: mkColor "black"
--     , width: 0.025
--     }
 
mkRGraph ::
  { vertices :: Array (VIn ( x :: Number, y :: Number))
  , edges :: Array (EIn ())} ->
  Graph (RV ()) (RE ())
mkRGraph = mkGraph >>> defaultRenderProps

renderVertex :: forall v p i. RV v -> SVG p i
renderVertex v = 
  circle [cx v.x, cy v.y, r v.radius, class_ (String.joinWith " " v.classes)] []
  -- circle [cx v.x, cy v.y, fill v.color, stroke v.strokeColor, strokeWidth v.strokeWidth, r v.radius] []

renderLinearEdge :: forall v e p i. RE e -> RV v -> RV v -> SVG p i
renderLinearEdge e s t =
  path [d ed, class_ (String.joinWith " " e.classes)] []
  -- path [d ed, stroke e.color, fill "none", strokeWidth e.width] []
  where
  ed = "M" <> show s.x <> "," <> show s.y <> "L" <> show t.x <> "," <> show t.y

renderCircularLoop :: forall v e p i. CircularLoopProps -> RE e -> RV v -> RV v -> SVG p i
renderCircularLoop p e s t =
  circle 
    [ cx (s.x + p.radius * (cos p.angle))
    , cy (s.y + p.radius * (sin p.angle))
    , r p.radius
    , class_ (String.joinWith " " e.classes)
    ] []
  -- circle 
  --   [ cx (s.x + p.radius * (cos p.angle))
  --   , cy (s.y + p.radius * (sin p.angle))
  --   , r p.radius
  --   , fill "none"
  --   , stroke e.color
  --   , strokeWidth e.width
  --   ] []

renderEdge :: forall v e p i. RE e -> RV v -> RV v -> SVG p i
renderEdge e = case e.renderType of
  LinearEdge -> renderLinearEdge e
  CircularLoop props -> renderCircularLoop props e

render :: forall v e p i. Graph (RV v) (RE e) -> SVG p i
render = render' renderVertex renderEdge

highlightEdges :: forall v e. String -> Array String -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
highlightEdges color edges = mapE $ \e -> 
  Array.elemIndex (unEdge e.id) edges
  -- # maybe e (\_ -> e{color = color, width = e.width*1.2})
  # maybe e (\_ -> e{classes = Array.snoc e.classes ("highlight-" <> color)})

thickenEdges :: forall v e. Array String -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
thickenEdges edges = mapE $ \e -> 
  Array.elemIndex (unEdge e.id) edges
  -- # maybe e (\_ -> e{color = color, width = e.width*1.2})
  # maybe e (\_ -> e{classes = Array.snoc e.classes ("thick")})

lightenEdges :: forall v e. Array String -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
lightenEdges edges = mapE $ \e -> 
  Array.elemIndex (unEdge e.id) edges
  -- # maybe e (\_ -> e{color = color, width = e.width*1.2})
  # maybe e (\_ -> e{classes = Array.snoc e.classes ("thin")})

lightenOtherEdges :: forall v e. Array String -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
lightenOtherEdges edges = mapE $ \e -> 
  Array.elemIndex (unEdge e.id) edges
  -- # maybe e (\_ -> e{color = color, width = e.width*1.2})
  # maybe (e{classes = Array.snoc e.classes ("thin")}) (\_ -> e)

circularLoop :: forall v e. String -> CircularLoopProps -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
circularLoop eStr props = mapE $ \e ->
  if e.id == eId
  then e{renderType = CircularLoop props}
  else e
  where eId = Edge eStr

scale :: forall v e. Number -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
scale s = mapV $ \v -> v{x = v.x * s, y = v.y * s}

translate :: forall v e. Number -> Number -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
translate dx dy = mapV $ \v -> v{x = v.x + dx, y = v.y + dy}

type BoundingBoxProps = 
  { left :: Number
  , right :: Number
  , top :: Number
  , bottom :: Number
  }

newtype BoundingBox = BoundingBox BoundingBoxProps

unBoundingBox :: BoundingBox -> BoundingBoxProps
unBoundingBox (BoundingBox b) = b

instance semigroupBoundingBox :: Semigroup BoundingBox where
  append (BoundingBox b1) (BoundingBox b2) = BoundingBox
    { left: min b1.left b2.left
    , right: max b1.right b2.right
    , top: min b1.top b2.top
    , bottom: max b1.bottom b2.bottom
    }

vertexBounds :: forall v. RV v -> BoundingBox
vertexBounds v = BoundingBox
  { left: v.x
  , right: v.x
  , top: v.y
  , bottom: v.y
  } 
  -- { left: v.x - v.radius
  -- , right: v.x + v.radius
  -- , top: v.y - v.radius
  -- , bottom: v.y + v.radius
  -- }

edgeBounds :: forall v e. RE e -> RV v -> RV v -> BoundingBox
edgeBounds e s t = case e.renderType of
  LinearEdge -> BoundingBox
    { left: min s.x t.x
    , right: max s.x t.x
    , top: min s.y t.y
    , bottom: max s.y t.y
    }
  CircularLoop p ->
    let cx = s.x + p.radius * (cos p.angle)
        cy = s.y + p.radius * (sin p.angle)
    in BoundingBox
    { left: cx - p.radius
    , right: cx + p.radius
    , top: cy - p.radius
    , bottom: cy + p.radius
    }

graphBounds :: forall v e. Graph (RV v) (RE e) -> Maybe BoundingBox
graphBounds g = vBounds <> eBounds
  where
  vBounds = foldMap (Just <<< vertexBounds) g.vertices
  eBounds = foldMap eBound g.edges
  eBound e = do
    ctx <- edgeContext e.id g
    pure $ edgeBounds e ctx.source ctx.target

autoTransform :: forall v e. BoundingBoxProps -> Graph (RV v) (RE e) -> Graph (RV v) (RE e)
autoTransform bounds g = fromMaybe g $ do
  gBounds <- graphBounds g <#> unBoundingBox
  let sx = (bounds.right - bounds.left) / (gBounds.right - gBounds.left)
      sy = (bounds.bottom - bounds.top) / (gBounds.bottom - gBounds.top)
      s = min sx sy
      dx = (bounds.left + bounds.right) / 2.0 - s * (gBounds.left + gBounds.right) / 2.0
      dy = (bounds.top + bounds.bottom) / 2.0 - s * (gBounds.top + gBounds.bottom) / 2.0
  pure $ g # scale (min sx sy) # translate dx dy

interpolateGraphs :: forall v e.  Graph (RV v) (RE e) -> Graph (RV v) (RE e) -> Number -> Graph (RV v) (RE e)
interpolateGraphs g1 g2 t = mergeVertices interpolateVertices g1 g2
  where
  interpolateVertices :: Maybe (RV v) -> Maybe (RV v) -> Maybe (RV v)
  interpolateVertices Nothing Nothing = Nothing
  interpolateVertices (Just v) Nothing = Just v
  interpolateVertices Nothing (Just v) = Just v
  interpolateVertices (Just v1) (Just v2) = Just v1
    { x = (1.0 - t) * v1.x + t * v2.x
    , y = (1.0 - t) * v1.y + t * v2.y
    , radius = (1.0 - t) * v1.radius + t * v2.radius
    }

interpolateSeq :: forall a.
  (a -> a -> Number -> a) -> 
  Array {time :: Number, value :: a} -> 
  Number -> Maybe a
interpolateSeq interpolate frames now = 
  maybe lastVal withI $ maybeI
  where
  n = Array.length frames
  frames' = frames # Array.sortBy (\{time:t1} {time:t2} -> compare t1 t2)
  firstVal = Array.index frames' 0 <#> _.value
  lastVal = Array.index frames' (n - 1) <#> _.value
  maybeI = frames' # Array.findIndex (\{time} -> time > now)
  withI i = fromMaybe firstVal $ do
    {time:t1,value:v1} <- Array.index frames' (i-1)
    {time:t2,value:v2} <- Array.index frames' i
    let now' = (now - t1) / (t2 - t1)
    pure $ Just $ interpolate v1 v2 now'

interpolateGraphSeq :: forall v e. 
  Array {time :: Number, value :: Graph (RV v) (RE e)} -> 
  Number -> Graph (RV v) (RE e)
interpolateGraphSeq graphs now = 
  fromMaybe emptyGraph $ interpolateSeq interpolateGraphs graphs now