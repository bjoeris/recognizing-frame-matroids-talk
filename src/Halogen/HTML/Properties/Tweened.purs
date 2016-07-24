module Halogen.HTML.Properties.Tweened where

import Prelude

import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.HTML.Properties.Indexed as I

import Timeline.Tween (TTween, ttween, ttween', tween, Tween)
import Timeline.Build (StepId(), Timeline())

-- class FromProp a r i where
--   fromProp :: IProp r i -> a

-- instance fromPropIProp :: FromProp (IProp r i) r i where
--   fromProp = id

-- instance fromPropTTweenIProp :: FromProp (Timeline b (Tween b (IProp r i))) r i where
--   fromProp = pure >>> pure

ttweenProp :: forall b a r i. 
  (a -> IProp r i) -> 
  (Number -> a) -> 
  TTween b (IProp r i)
ttweenProp mkProp val = 
  ttween (val >>> mkProp)
  
ttweenProp' :: forall b a r i.
  StepId b ->
  (a -> IProp r i) -> 
  (Number -> a) -> 
  TTween b (IProp r i)
ttweenProp' id mkProp val = 
  ttween' id (val >>> mkProp)
  
tweenProp :: forall b a r i.
  StepId b ->
  (a -> IProp r i) -> 
  (Number -> a) -> 
  Tween b (IProp r i)
tweenProp id mkProp val = 
  tween id (val >>> mkProp)

class_ :: forall b r i. ClassName -> TTween b (IProp ("class" :: I | r) i)
class_ = I.class_ >>> pure >>> pure