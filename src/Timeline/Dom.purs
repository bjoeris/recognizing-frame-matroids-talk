module Timeline.Dom where

-- import Prelude

-- import Data.Exists (Exists)
-- import Data.Traversable (sequence)
-- import Data.Maybe (Maybe(Just,Nothing))

-- import Unsafe.Coerce (unsafeCoerce)

-- import Halogen.HTML.Core (Prop(), prop, propName, propNames, attrName)
-- import Halogen.HTML.Indexed as HH
-- import Halogen.HTML.Properties.Indexed (IProp(), I(), InteractiveEvents, GlobalProperties)

-- import Timeline.Tween (TTween, Tween, tween)

-- type TTweenNode b r p i
--    = Array (TTween b (IProp (InteractiveEvents (GlobalProperties r)) i))
--     -> Array (TTween b (HH.HTML p i))
--     -> TTween b (HH.HTML p i)

-- ttweenNode :: forall b a c r. (Array a -> Array c -> r) -> Array (TTween b a) -> Array (TTween b c) -> TTween b r
-- ttweenNode f attrs children = do
--   attrs' <- sequence <$> sequence attrs
--   children' <- sequence <$> sequence children
--   pure $ f <$> attrs' <*> children'

-- text :: forall b p i. String -> TTween b (HH.HTML p i)
-- text = HH.text >>> pure >>> pure

-- div :: forall b p i. TTweenNode b (onScroll :: I) p i
-- div = ttweenNode HH.div

-- styleU :: forall i. String -> Prop i
-- styleU = prop (propName "style") (Just $ attrName "style")

-- styleI :: forall r i. String -> IProp ( style :: I | r) i
-- styleI = unsafeCoerce styleU

-- style :: forall b r i. String -> TTween b (IProp ( style :: I | r) i)
-- style = styleI >>> pure >>> pure

-- style_opacityU :: forall i. Number -> Prop i
-- style_opacityU = prop (propNames ["style", "opacity"]) Nothing

-- style_opacityI :: forall r i. Number -> IProp ( style :: I | r) i
-- style_opacityI = unsafeCoerce style_opacityU

-- style_opacity :: forall b r i. Number -> TTween b (IProp ( style :: I | r) i)
-- style_opacity = style_opacityI >>> pure >>> pure

-- tweenProp :: forall b a r i. 
--   (a -> IProp ( style :: I | r) i) -> 
--   (Number -> a) -> 
--   TTween b (IProp ( style :: I | r) i)
-- tweenProp mkProp val = 
--   tween (val >>> mkProp)

-- tweenStyle :: forall b r i. (Number -> String) -> TTween b (IProp ( style :: I | r) i)
-- tweenStyle = tweenProp styleI

-- tweenOpacity :: forall b r i. (Number -> Number) -> TTween b (IProp (style :: I | r) i)
-- tweenOpacity = tweenProp style_opacityI