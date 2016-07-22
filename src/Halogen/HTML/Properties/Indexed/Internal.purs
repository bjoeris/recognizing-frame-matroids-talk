module Halogen.HTML.Properties.Indexed.Internal (refine) where

import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Core (Prop())
import Halogen.HTML.Properties.Indexed (IProp())

refine :: forall a r i. (a -> Prop i) -> a -> IProp r i
refine = unsafeCoerce