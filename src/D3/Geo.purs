module D3.Geo where

import Prelude

import Data.Foreign (Foreign, ForeignError(JSONError), toForeign, F())
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Either (Either(Left))
import Data.Foreign.Generic (readGeneric, toForeignGeneric, defaultOptions, SumEncoding(TaggedObject))
import Data.Foreign.Generic as Data.Foreign.Generic
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Nothing,Just))
import Math (sqrt)
import Data.Nullable (Nullable, toNullable)
import Partial.Unsafe (unsafePartial)
import Data.Either (fromRight)

foreignOptions :: Data.Foreign.Generic.Options
foreignOptions = defaultOptions 
  { sumEncoding = TaggedObject
    { tagFieldName : "type"
    , contentsFieldName : "coordinates"
    }
  , unwrapNewtypes = true
  }

newtype Rotation = Rotation (Array Number)
derive instance rotationGeneric :: Generic Rotation

instance rotationIsForeign :: IsForeign Rotation where
  read = readGeneric foreignOptions

rotation :: Number -> Number -> Number -> Rotation
rotation lambda phi gamma = Rotation [lambda, phi, gamma]

newtype Coord = Coord (Array Number)
derive instance coordGeneric :: Generic Coord

instance coordIsForeign :: IsForeign Coord where
  read = readGeneric foreignOptions

coord :: Number -> Number -> Coord
coord x y = Coord [x, y]

newtype Extent = Extent (Array (Array Number))
extent :: Number -> Number -> Number -> Number -> Extent
extent left top right bottom = Extent [[left, top], [right, bottom]]

data Geometry
  = Point Coord
  | LineString (Array Coord)
  | Polygon (Array (Array Coord))
  | MultiPoint (Array Coord)
  | MultiLineString (Array (Array Coord))
  | MultiPolygon (Array (Array (Array Coord)))
  | Sphere
derive instance geometryGeneric :: Generic Geometry

geometryToForeign :: Geometry -> Foreign
--geometryToForeign = toForeignGeneric foreignOptions
geometryToForeign g = case g of
  Point c -> export "Point" c
  LineString c -> export "LineString" c
  Polygon c -> export "Polygon" c
  MultiPoint c -> export "MultiPoint" c
  MultiLineString c -> export "MultiLineString" c
  MultiPolygon c -> export "MultiPolygon" c
  Sphere -> toForeign { type : "Sphere" }
  where
  export :: forall a. String -> a -> Foreign 
  export t c = toForeign { type : t, coordinates : c }

instance geometryIsForeign :: IsForeign Geometry where
--  read = readGeneric foreignOptions
  read value = do
    t <- readProp "type" value
    let coords :: forall a. IsForeign a => F a
        coords = readProp "coordinates" value
    case t of
      "Point" -> Point <$> coords
      "LineString" -> LineString <$> coords
      "Polygon" -> Polygon <$> coords
      "MultiPoint" -> MultiPoint <$> coords
      "MultiLineString" -> MultiLineString <$> coords
      "MuiltyPolygon" -> MultiPolygon <$> coords
      "Sphere" -> pure Sphere
      _ -> Left $ JSONError $ "Invalid geometry type '" <> t <> "'." 

type GeoCircleProps = { center :: Coord, radius :: Number, precision :: Number }

geoCircleProps :: GeoCircleProps
geoCircleProps = 
  { center : coord 0.0 0.0
  , radius : 90.0
  , precision : 6.0
  }

foreign import geoCircleImpl :: GeoCircleProps -> Foreign

geoCircle' :: GeoCircleProps -> Geometry
geoCircle' props = geoCircleImpl props # read # (unsafePartial fromRight)

geoCircle :: Coord -> Number -> Geometry
geoCircle c r = 
  geoCircle' geoCircleProps { center = c, radius = r }

foreign import geoDistance :: Coord -> Coord -> Number

foreign import data GeoProjection :: *

foreign import geoOrthographic :: GeoProjection

foreign import clipAngleImpl :: Nullable Number -> GeoProjection -> GeoProjection

clipAngle :: Maybe Number -> GeoProjection -> GeoProjection
clipAngle = toNullable >>> clipAngleImpl

foreign import clipExtentImpl :: Nullable Extent -> GeoProjection -> GeoProjection
clipExtent :: Maybe Extent -> GeoProjection -> GeoProjection
clipExtent = toNullable >>> clipExtentImpl

foreign import scale :: Number -> GeoProjection -> GeoProjection
foreign import translate :: Coord -> GeoProjection -> GeoProjection
foreign import center :: Number -> GeoProjection -> GeoProjection
foreign import rotate :: Rotation -> GeoProjection -> GeoProjection
foreign import precision :: Number -> GeoProjection -> GeoProjection

foreign import data GeoPath :: *

foreign import geoPath :: GeoPath

foreign import pointRadius :: Number -> GeoPath -> GeoPath

foreign import projectionImpl :: Nullable GeoProjection -> GeoPath -> GeoPath

projection :: Maybe GeoProjection -> GeoPath -> GeoPath
projection = toNullable >>> projectionImpl

foreign import pathStringImpl :: GeoPath -> Foreign -> String

pathString :: GeoPath -> Geometry -> String
pathString path = pathStringImpl path <<< geometryToForeign





-- type GeoProjectionProps = 
--   { clipAngle :: Maybe Number
--   , clipExtent :: Maybe Number
--   , scale :: Maybe Number
--   , translate :: Coord
--   , center :: Coord
--   , rotate :: Rotation
--   , precision :: Number
--   }

-- geoProjectionProps :: GeoProjectionProps
-- geoProjectionProps = 
--   { clipAngle : Nothing
--   , clipExtent : Nothing
--   , scale : Nothing
--   , translate : coord 0.0 0.0
--   , center : coord 0.0 0.0
--   , rotate : rotation 0.0 0.0 0.0
--   , precision : sqrt 0.5 
--   } 

-- foreign import geoOrthographic :: GeoProjectionProps -> GeoProjection

-- foreign import data GeoPath :: *

-- type GeoPathProps =
--   { projection :: Maybe GeoProjection
--   , pointRadius :: Number
--   }

-- geoPathProps :: GeoPathProps
-- geoPathProps =
--   { projection : Nothing
--   , pointRadius : 4.5
--   }

-- foreign import geoPath :: GeoPathProps -> GeoPath

-- foreign import pathStringImpl :: GeoPath -> Foreign -> String

-- pathString :: GeoPath -> Geometry -> String
-- pathString path = pathStringImpl path <<< geometryToForeign
