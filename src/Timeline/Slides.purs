module Timeline.Slides where

import Prelude

import Halogen.HTML.Core(HTML(Text,Element), PropName, propNames, runPropName, Prop(Prop), PropF(PropF), ClassName(), className)
import Halogen.HTML.Elements.Tweened(div)
import Halogen.HTML.Properties.Tweened(class_)
import Halogen.HTML.Indexed as I
import Halogen.HTML.Properties.Indexed as I
import Halogen.HTML.Styles.Indexed as I
import Halogen.HTML.Styles as P
import Halogen.HTML.Properties as P

import Data.Array as Array
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)
import Data.Traversable (mapAccumL, Accum, traverse, sequence)
import Data.Exists (Exists, runExists, mkExists)

import Control.Bind (bindFlipped)
import Control.Monad.State.Trans (StateT, evalStateT, get, put, lift)

import Timeline.Build(newStep, StepId, Timeline)
import Timeline.Tween (TTween, Tween, tween)

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace as Debug

data FadeDirection
  = FadeIn
  | FadeOut

updateProp :: forall v p i. PropName v -> (v -> Prop i) -> (Maybe v -> Maybe v) -> HTML p i -> HTML p i
updateProp propName newProp fn (Element ns tag props els) = Element ns tag props' els
  where
  props' = Array.catMaybes propMaybes
  propMaybes = 
    if res.accum
    then res.value
    else Array.snoc res.value (newProp <$> fn Nothing)  
  res = mapAccumL tryProp false props
  tryProp :: Boolean -> Prop i -> Accum Boolean (Maybe (Prop i))
  tryProp accum (Prop ex) = ex # runExists (tryPropF accum)
  tryProp accum attr = { accum, value: Just attr }
  tryPropF :: forall v'. Boolean -> PropF v' -> Accum Boolean (Maybe (Prop i))
  tryPropF accum propF@(PropF propName' value attr) =
    if runPropName propName' == runPropName propName
    then { accum: true
         , value: do 
             value' <- fn (unsafeCoerce $ Just value)
             pure $ Prop $ mkExists $ PropF propName' (unsafeCoerce value') attr
         }
    else { accum
         , value: Just $ Prop $ mkExists propF }
  updatePropF :: forall v'. v' -> PropF v' -> PropF v'
  updatePropF value' (PropF propName' _ attr) =
    PropF propName' value' attr
updateProp _ _ _ node = node

transitionProp :: forall v b p i. PropName v -> (v -> Prop i) -> Tween b (Maybe v -> Maybe v) -> TTween b (HTML p i) -> TTween b (HTML p i)
transitionProp name newProp tf = map (bindFlipped go)
  where
  go tc = do
    fn <- tf
    pure $ updateProp name newProp fn tc

-- transitionProps :: forall b p i. Tween (Array (Prop i)) -> TTween b (HTML p i) -> TTween b (HTML p i)
-- transitionProps getProps = map (bindFlipped go)
--   where
--   go (Text x) = do
--     props <- getProps
--     pure $ P.span props [Text x]
--   go (Element ns tag props children) = do
--     props' <- getProps
--     pure $ Element ns tag (props <> props') children
--   go x = pure x

fade :: forall b p i. FadeDirection -> StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
fade d i = transitionProp (propNames ["style", "opacity"]) P.opacity (tween i updateOpacity) 
  where
  updateOpacity t Nothing =
    Just $ case d of
      FadeIn -> t
      FadeOut -> 1.0 - t
  updateOpacity t (Just v) =
    if t > 0.0
    then updateOpacity t Nothing
    else Just v

fadeIn' :: forall b p i. StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
fadeIn' = fade FadeIn

fadeOut' :: forall b p i. StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
fadeOut' = fade FadeOut

fadeIn :: forall b p i. TTween b (HTML p i) -> TTween b (HTML p i)
fadeIn content = do
  i <- newStep
  fadeIn' i content

fadeOut :: forall b p i. TTween b (HTML p i) -> TTween b (HTML p i)
fadeOut content = do
  c <- content
  i <- newStep
  fadeOut' i (pure c)

fadeInOut' :: forall b p i. StepId b -> StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
fadeInOut' i j content = fadeOut' j $ fadeIn' i content

fadeInOut :: forall b p i. TTween b (HTML p i) -> TTween b (HTML p i)
fadeInOut content = do
  i <- newStep
  c <- content
  j <- newStep
  fadeInOut' i j (pure c)

hide :: forall b p i. FadeDirection -> StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
hide dir i = map (bindFlipped go)
  where
  go (Element ns tag props els) = do
    t <- tween i $ \t' -> case dir of
      FadeIn -> t'
      FadeOut -> 1.0 - t'
    let props' = 
          if t <= 0.0
          then (Array.snoc props (P.class_ (className "hidden")))
          else props
    pure $ Element ns tag props' els
  go node = pure node

hideBefore :: forall b p i. StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
hideBefore = hide FadeIn

hideAfter :: forall b p i. StepId b -> TTween b (HTML p i) -> TTween b (HTML p i)
hideAfter = hide FadeOut

-- TODO: make sure the steps are generated in the right order. Need to execute the content between the fadein/fadeout steps
slides :: forall b p i. Array (TTween b (HTML p i)) -> TTween b (HTML p i)
slides content = do
  content' <- getContent'
  pure $ I.div [I.class_ (className "slide-background")] <$> content'
  where
  getContent' :: TTween b (Array (HTML p i))
  getContent' = do
    i0 <- newStep
    content' <- evalStateT (traverse go content) i0
    pure $ sequence content'
  go :: TTween b (HTML p i) -> StateT (StepId b) (Timeline b) (Tween b (HTML p i))
  go getSlideContent = do
    slideContent <- lift getSlideContent
    i <- get
    j <- lift newStep
    put j
    lift $ fadeSlide i j slideContent
  fadeSlide :: StepId b -> StepId b -> Tween b (HTML p i) -> TTween b (HTML p i)
  fadeSlide prevStep nextStep slideContent =
    fadeOut' nextStep $ hideAfter nextStep $
    fadeIn' prevStep $ hideBefore prevStep $
    div [ class_ (className "slide-background")]
          [ div [class_ (className "slide")]
          [ div [class_ (className "slide-content")]
                  [ pure $ slideContent ] ] ]