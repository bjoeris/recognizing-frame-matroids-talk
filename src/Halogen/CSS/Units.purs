module Halogen.CSS.Units where

import Prelude (show)

import Data.Semigroup ((<>))

newtype UnitSuffix u = UnitSuffix String

unUnitSuffix :: forall u. UnitSuffix u -> String
unUnitSuffix (UnitSuffix s) = s

class IsUnit a where
  unitSuffix :: UnitSuffix a

data Em 
instance isUnitEm :: IsUnit Em where
  unitSuffix = UnitSuffix "em"

data Px

instance isUnitPx :: IsUnit Px where
  unitSuffix = UnitSuffix "px"

px :: Number -> Length Px
px = Length

data Length u = Length Number

class IsLength a where
  toLengthString :: a -> String

instance isLengthNumber :: IsLength Number where
  toLengthString = show

instance isUnitIsLength :: IsUnit u => IsLength (Length u) where
  toLengthString (Length n) = (show n) <> (unUnitSuffix (unitSuffix :: UnitSuffix u))