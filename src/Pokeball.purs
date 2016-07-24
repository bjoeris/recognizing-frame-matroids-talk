module Pokeball where

import Prelude hiding (id)

import Data.Maybe(Maybe(Just,Nothing), fromMaybe)
import Data.Array as Array
import Data.Time.Duration (Milliseconds, Seconds(Seconds))

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

import Math (pi)

pokeball :: forall b p i. TTween b (HTML p i)
pokeball = 
  pokeball' <$>
    Timeline.newStep <*>
    Timeline.newStep <*>
    Timeline.newStep <*>
    Timeline.newStep <*>
    Timeline.newStep' Timeline.StepEnd (Seconds 1.0) 
  where
  width = 50.0 # rem
  height = 50.0 # rem
  rotation = D3.rotation (-15.0) 100.0 0.0
  radius = 0.95
  buttonRadius = 15.0
  vertexRadius = 6.0
  edgeWidth = 0.04
  hiddenEdgeWidth = 0.005
  projection = geoOrthographic 
    # translate (coord 0.0 0.0)
    # scale radius
    # clipAngle (Just 90.0)
    # rotate rotation
  backProjection = projection # clipAngle (Just 180.0)
  pathGen = pathString (geoPath # D3.projection (Just projection))
  backPathGen = pathString (geoPath # D3.projection (Just backProjection))
  sphere = pathGen $ Sphere
  vertex1Center = coord 90.0 (-90.0 + buttonRadius)
  vertex2Center = coord (-90.0) (-90.0 + buttonRadius)
  vertex3Center = coord 0.0 (-90.0 + buttonRadius)
  -- white = "white"
  -- red = "red"
  white = "url(#white-gradient)"
  red = "url(#red-gradient)"

  pokeball' i0 i1 i2 i3 i4 = do
    t0 <- tween i0 (\t -> t)
    t1 <- tween i1 (\t -> t)
    t2 <- tween i2 (\t -> t)
    t3 <- tween i3 (\t -> t)
    t4 <- tween i4 (\t -> t)
    --face1Center <- tween i4 (\t -> coord 0.0 (-70.0 * (1.0 - t)))

    let face1CenterY = -70.0 * (1.0 - t4)
        face1Center = coord 0.0 face1CenterY
        distRad = geoDistance face1Center vertex1Center
        distDeg = distRad * 180.0 / pi
        vertex4Center = coord 0.0 (face1CenterY + distDeg)
        face1Geo = geoCircle face1Center distDeg
        path1Geo = LineString $ case face1Geo of
          Polygon c -> Array.index c 0 # fromMaybe []
          _ -> []
        face1 = pathGen face1Geo
        path1 = pathGen path1Geo
        backPath1 = backPathGen path1Geo
        face2 = pathGen $ geoCircle (coord 0.0 (-90.0)) buttonRadius
        path2 = pathGen $ geoCircle (coord 0.0 (-90.0)) buttonRadius
        path3 = pathGen $ LineString [ vertex3Center, vertex4Center ]
        vertexPaths = map (\v -> 
          path [d $ pathGen $ geoCircle v vertexRadius
               , fill "black"
               , stroke white
               , strokeWidth (edgeWidth / 4.0)] [])
          [ vertex1Center
          , vertex2Center
          , vertex3Center
          , vertex4Center ]
        highlight = pathGen $ geoCircle (coord (-40.0) (-20.0)) 20.0
        lowlight = pathGen $ geoCircle (coord (-10.0) (-200.0)) 50.0
        gradientProps =
          [ x1 (-0.7)
          , y1 (-0.7)
          , x2 1.2
          , y2 1.2
          , gradientUnits userSpaceOnUse
          ]

    pure $ svg [ SVG.width width, 
                 SVG.height height, 
                 viewBox (-1.0) (-1.0) 2.0 2.0, 
                 preserveAspectRatio {defer: false, align: Just {x:Mid, y:Mid}, meetOrSlice: Nothing} ]
      [ defs [] 
        [ E.filter [id "f1", x 0.0, y 0.0] 
          [ feGaussianBlur [in_ "SourceGraphic", stdDeviation 0.1] [] ]
        , E.clipPath [id "clip-sphere"] 
          [ path [d sphere] [] ]
        , linearGradient ([id "red-gradient"] <> gradientProps)
          [ stop [ offset 0.0, stopColor "#ff3a37"] []
          , stop [ offset 1.0, stopColor "#501010"] [] ]
        , linearGradient ([id "white-gradient"] <> gradientProps)
          [ stop [ offset 0.0, stopColor "#f8f8f8"] []
          , stop [ offset 1.0, stopColor "#909090"] [] ] ]
      , path [d sphere, fill white] []
      , path [d sphere, fill red, opacity (t2-t3)] []

      , path [d face1, fill white] []
      , path [d face1, fill red, opacity t3] []
      --, path [d backPath1, stroke "black", strokeWidth hiddenEdgeWidth, opacity 0.3, fill "none"] []
      , path [d path1, stroke "black", strokeWidth edgeWidth, strokeLinecap round, fill "none", P.clipPath "url(#clip-sphere)"] []

      , path [d face2, fill white] []
      , path [d face2, fill red, opacity (t1-t2)] []
      , path [d path2, stroke "black", strokeWidth edgeWidth, fill "none"] []

      , path [d path3, stroke "black", strokeWidth edgeWidth, fill "none", opacity (1.0-t0)] []
      
      , g [opacity (1.0-t4)] vertexPaths
      , path [d highlight, fill "white", opacity 0.9, P.filter "url(#f1)", P.clipPath "url(#clip-sphere)"] []
      --, path [d lowlight, fill "black", opacity 0.2, P.filter "url(#f1)", P.clipPath "url(#clip-sphere)"] []
      ]

-- mapMap :: forall a b f g. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
-- mapMap = map <<< map

-- infixl 4 mapMap as <$$> 

-- mapApply :: forall a b f g. (Apply f, Apply g) => f (g (a -> b)) -> f (g a) -> f (g b)
-- mapApply = apply <<< map apply

-- infixl 4 mapApply as <$*>

-- pokeball :: forall b p i. TTween b (HTML p i)
-- pokeball = do
--   i <- Timeline.newStep' Timeline.StepEnd (Seconds 1.0)

--   let face1Center = tween' i (\t -> coord 0.0 (-70.0 * (1.0 - t)))
--       distRad = geoDistance <$$> face1Center <$*> pure vertex1Center
--       face1Geo = geoCircle <$> face1Center <*> (distRad * 180.0 / pi)
--       path1Geo = polygonToLineString <$> face1Geo
--       face1 = pathGen <$> face1Geo
--       path1 = pathGen <$> path1Geo
--       backPath1 = backPathGen <$> path1Geo
--       face2 = pure $ pathGen $ geoCircle (coord 0.0 (-90.0)) buttonRadius
--       path2 = pure $ pathGen $ geoCircle (coord 0.0 (-90.0)) buttonRadius
--       vertex1 = pure $ pathGen $ geoCircle (coord 90.0 (-90.0 + buttonRadius)) vertexRadius
--       vertex2 = pure $ pathGen $ geoCircle (coord 90.0 (-90.0 - buttonRadius)) vertexRadius
--       highlight = pure $ pathGen $ geoCircle (coord (-40.0) (-20.0)) 30.0
--       lowlight = pure $ pathGen $ geoCircle (coord (-10.0) (-200.0)) 50.0

--   svg [SVG.width width, SVG.height height]
--   [ defs [] 
--     [ E.filter [id "f1", x 0.0, y 0.0] 
--       [ feGaussianBlur [in_ "SourceGraphic", stdDeviation 8.0] [] ]
--     , E.clipPath [id "clip1"] 
--       [ path [d sphere] [] ] ]
--   , path [d sphere, fill "white"] []
--   , path [d face1, fill "red"] []
--   , path [d backPath1, stroke "black", strokeWidth 1.0, opacity 0.3, fill "none"] []
--   , path [d path1, stroke "black", strokeWidth 8.0, strokeLinecap round, fill "none"] []
--   , path [d face2, fill "white"] []
--   , path [d path2, stroke "black", strokeWidth 8.0, fill "none"] []
--   , path [d vertex1, fill "black"] []
--   , path [d vertex2, fill "black"] []
--   , path [d highlight, fill "white", opacity 0.4, P.filter "url(#f1)", P.clipPath "url(#clip1)"] []
--   , path [d lowlight, fill "black", opacity 0.2, P.filter "url(#f1)", P.clipPath "url(#clip1)"] []
--   ]
--   where
--   rotation = D3.rotation (-15.0) 100.0 0.0
--   width = 500.0
--   height = 500.0
--   radius = height / 2.0 - 5.0
--   buttonRadius = radius / 20.0
--   vertexRadius = 3.0
--   projection = geoOrthographic 
--     # translate (coord (width / 2.0) (height / 2.0))
--     # scale radius
--     # clipAngle (Just 90.0)
--     # rotate rotation
--   backProjection = projection # clipAngle (Just 180.0)
--   pathGen = pathString (geoPath # D3.projection (Just projection))
--   backPathGen = pathString (geoPath # D3.projection (Just backProjection))
--   sphere = pathGen $ Sphere
--   vertex1Center = coord 90.0 (-90.0 + buttonRadius)
--   polygonToLineString poly = LineString $ case poly of
--     Polygon c -> Array.index c 0 # fromMaybe []
--     _ -> []
