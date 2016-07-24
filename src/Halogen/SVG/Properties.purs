module Halogen.SVG.Properties where

import Prelude

import Data.Maybe (Maybe(Just,Nothing), maybe)
import Data.Exists (mkExists)
import Data.Tuple (Tuple(Tuple))

import Halogen.HTML.Core (Prop(Prop,Attr), PropF(PropF), ClassName(), PropName(), AttrName(), prop, propName, attrName, runClassName, class IsProp )

import Halogen.SVG.Elements (svgNS)

import Halogen.CSS.Units (class IsLength, toLengthString)
import Halogen.CSS.Colors (class IsColor, toColorString)

attr :: forall i. String -> String -> Prop i
attr = Attr Nothing <<< attrName

class_ :: forall i. String -> Prop i
class_ = attr "class"

d :: forall i. String -> Prop i
d = attr "d"

lengthAttr :: forall v i. IsLength v => String -> v -> Prop i
lengthAttr name v =
  Attr Nothing (attrName name) (toLengthString v)
  
colorAttr :: forall v i. IsColor v => String -> v -> Prop i
colorAttr name v =
  Attr Nothing (attrName name) (toColorString v)
  
numAttr :: forall i. String -> Number -> Prop i
numAttr name v =
  Attr Nothing (attrName name) (show v)

cx :: forall v i. IsLength v => v -> Prop i
cx = lengthAttr "cx"

cy :: forall v i. IsLength v => v -> Prop i
cy = lengthAttr "cy"

r :: forall v i. IsLength v => v -> Prop i
r = lengthAttr "r"

width :: forall v i. IsLength v => v -> Prop i
width = lengthAttr "width"

height :: forall v i. IsLength v => v -> Prop i
height = lengthAttr "height"

fill :: forall v i. IsColor v => v -> Prop i
fill = colorAttr "fill"

fillOpacity :: forall i. Number -> Prop i
fillOpacity = numAttr "fill-opacity"

stroke :: forall v i. IsColor v => v -> Prop i
stroke = colorAttr "stroke"

strokeWidth :: forall v i. IsLength v => v -> Prop i
strokeWidth = lengthAttr "stroke-width"

filter :: forall i. String -> Prop i
filter = attr "filter"

id :: forall i. String -> Prop i
id = attr "id"

x :: forall v i. IsLength v => v -> Prop i
x = lengthAttr "x"

x1 :: forall v i. IsLength v => v -> Prop i
x1 = lengthAttr "x1"

x2 :: forall v i. IsLength v => v -> Prop i
x2 = lengthAttr "x2"

y :: forall v i. IsLength v => v -> Prop i
y = lengthAttr "y"

y1 :: forall v i. IsLength v => v -> Prop i
y1 = lengthAttr "y1"

y2 :: forall v i. IsLength v => v -> Prop i
y2 = lengthAttr "y2"

in_ :: forall i. String -> Prop i
in_ = attr "in"

stdDeviation :: forall i. Number -> Prop i
stdDeviation = numAttr "stdDeviation"

clipPath :: forall i. String -> Prop i
clipPath = attr "clip-path"

data Linecap
  = LinecapButt
  | LinecapRound
  | LinecapSquare


data Linejoin
  = LinejoinMiter
  | LinejoinRound
  | LinejoinBevel

butt :: Linecap
butt = LinecapButt

class IsRound a where
  round :: a

instance isRoundLinecap :: IsRound Linecap where
  round = LinecapRound

square :: Linecap
square = LinecapSquare 

miter :: Linejoin
miter = LinejoinMiter

instance isRoundLinejoin :: IsRound Linejoin where
  round = LinejoinRound

bevel :: Linejoin
bevel = LinejoinBevel

strokeLinecap :: forall i. Linecap -> Prop i
strokeLinecap cap = 
  Attr Nothing (attrName "stroke-linecap") $ case cap of
    LinecapButt -> "butt"
    LinecapRound -> "round"
    LinecapSquare -> "square"
    
strokeLinejoin :: forall i. Linejoin -> Prop i
strokeLinejoin join = 
  Attr Nothing (attrName "stroke-linejoin") $ case join of
    LinejoinMiter -> "miter"
    LinejoinRound -> "round"
    LinejoinBevel -> "bevel"

offset :: forall i. Number -> Prop i
offset = numAttr "offset"

stopColor :: forall v i. IsColor v => v -> Prop i
stopColor = colorAttr "stop-color"

viewBox :: forall i. Number -> Number -> Number -> Number -> Prop i
viewBox x1 y1 x2 y2 =
  Attr Nothing (attrName "viewBox") (show x1 <> " " <> show y1 <> " " <> show x2 <> " " <> show y2)

data Align
  = Min
  | Mid
  | Max

data MeetOrSlice
  = Meet
  | Slice

type PreserveAspectRatio =
  { defer :: Boolean
  , align :: Maybe { x :: Align, y :: Align }
  , meetOrSlice :: Maybe MeetOrSlice }

toPreserveAspectRatioString :: PreserveAspectRatio -> String
toPreserveAspectRatioString {defer,align,meetOrSlice} =
  sDefer <> sFullAlign <> sMeetOrSlice
  where
  sDefer = if defer
    then "defer "
    else ""
  sFullAlign = align # maybe "none" (\{x,y} -> "x" <> sAlign x <> "Y" <> sAlign y)
  sAlign Min = "Min"
  sAlign Mid = "Mid"
  sAlign Max = "Max"
  sMeetOrSlice = case meetOrSlice of
    Nothing -> ""
    Just Meet -> " meet"
    Just Slice -> " slice"

preserveAspectRatio :: forall i. PreserveAspectRatio -> Prop i
preserveAspectRatio = 
  Attr Nothing (attrName "preserveAspectRatio") <<< toPreserveAspectRatioString

data GradientUnits
  = UserSpaceOnUse
  | ObjectBoundingBox

userSpaceOnUse :: GradientUnits
userSpaceOnUse = UserSpaceOnUse

objectBoundingBox :: GradientUnits
objectBoundingBox = ObjectBoundingBox

gradientUnits :: forall i. GradientUnits -> Prop i
gradientUnits u = 
  Attr Nothing (attrName "gradientUnits") $ case u of
    UserSpaceOnUse -> "userSpaceOnUse"
    ObjectBoundingBox -> "objectBoundingBox"

-- d :: forall i. PropName String -> Prop i
-- d = prop (propName "d") (Just $ attrName "d")

-- lengthProp :: forall v i. IsLength v => PropName String -> Maybe AttrName -> v -> Prop i
-- lengthProp name attr v =
--   Prop (mkExists (PropF name (toLengthString v) (flip Tuple (\_ _ s -> s) <$> attr)))

-- colorProp :: forall v i. IsColor v => PropName String -> Maybe AttrName -> v -> Prop i
-- colorProp name attr v =
--   Prop (mkExists (PropF name (toColorString v) (flip Tuple (\_ _ s -> s) <$> attr)))

-- cx :: forall v i. IsLength v => v -> Prop i
-- cx = lengthProp (propName "cx") (Just $ attrName "cx")

-- cy :: forall v i. IsLength v => v -> Prop i
-- cy = lengthProp (propName "cy") (Just $ attrName "cy")

-- r :: forall v i. IsLength v => v -> Prop i
-- r = lengthProp (propName "r") (Just $ attrName "r")

-- width :: forall v i. IsLength v => v -> Prop i
-- width = lengthProp (propName "width") (Just $ attrName "width")

-- height :: forall v i. IsLength v => v -> Prop i
-- height = lengthProp (propName "height") (Just $ attrName "height")

-- fill :: forall v i. IsColor v => v -> Prop i
-- fill = colorProp (propName "fill") (Just $ attrName "fill")

-- stroke :: forall v i. IsColor v => v -> Prop i
-- stroke = colorProp (propName "stroke") (Just $ attrName "stroke")

-- strokeWidth :: forall v i. IsLength v => v -> Prop i
-- strokeWidth = lengthProp (propName "strokeWidth") (Just $ attrName "stroke-width")
 