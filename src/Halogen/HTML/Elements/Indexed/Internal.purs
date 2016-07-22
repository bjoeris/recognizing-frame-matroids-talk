module Halogen.HTML.Elements.Indexed.Internal (refine) where

import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Core (HTML(), Prop())
import Halogen.HTML.Properties.Indexed (IProp())

refine :: forall r p i. (Array (Prop i) -> Array (HTML p i) -> HTML p i) -> Array (IProp r i) -> Array (HTML p i) -> HTML p i
refine = unsafeCoerce