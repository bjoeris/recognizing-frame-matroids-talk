module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlElementToElement) as DOM
import DOM.HTML.Window (document) as DOM

import Halogen as H
import Halogen.HTML.Core (Prop(), prop, propName, attrName)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I(), InteractiveEvents, GlobalProperties)
import Halogen.HTML.Properties.Tweened (ttweenProp)
import Halogen.Query.EventSource as ES
import Halogen.Util (awaitBody, runHalogenAff, selectElement)

import Keyboard as K
import RequestAnimationFrame as A

import Data.Char as CH
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Time.Duration (Milliseconds, Seconds(Seconds))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple))

import Routing as R
import Routing.Match as R
import Routing.Match.Class as R
import Routing.Hash as R

import Timeline.Build as Timeline
import Timeline.Tween as Timeline
import Timeline.Compile as Timeline
import Timeline.Run as Timeline
import Timeline.Tween (TTween, ttween')
import Timeline.Slides (fade, FadeDirection(FadeIn,FadeOut), slides)
-- import Timeline.Dom (text,div,tweenStyle,tweenOpacity)

import Halogen.HTML.Styles.Indexed as I
import Halogen.HTML.Elements.Tweened (div, text)
import Halogen.HTML.Styles.Tweened

import Slides as Slides

import KaTeX as KaTeX

import Debug.Trace

data Query a 
  = Init a
  | Move Timeline.Movement a
  | Tick Milliseconds a

type State = Timeline.State (H.ComponentHTML Query)

timeline :: forall b. TTween b (H.ComponentHTML Query)
timeline = do
  i0 <- Timeline.newStep
  res <- slides Slides.slides
  Timeline.newStep
  pure $ res
  --opacityTween <- tween (\t -> "opacity: " <> show t)
  {-i1 <- Timeline.newStep
  fade FadeIn i1 $ div [] 
    [ div [ttweenProp I.opacity (\t -> t)]
        [ text "hello" ]
    , div [] [ pokeball ]
    ]-}
  -- styleTween <- style <$> opacityTween
  -- attributes <- sequence $ sequence [styleTween] 
  -- pure $ (HH.div <$> attributes
  --   <*> pure [ HH.text "hello" ])
    
initialState :: State
initialState = Timeline.initialState timeline

-- | Effects embedding the Ace editor requires.
type E eff = (dom :: DOM, avar :: AVAR, keyboard :: K.KEYBOARD | eff)

ui :: forall eff. H.Component State Query (Aff (E eff))
ui = H.lifecycleComponent { render, eval, initializer, finalizer: Nothing }
  where

  initializer :: Maybe (Query Unit)
  initializer = Just (H.action Init)

  render :: State -> H.ComponentHTML Query
  render = Timeline.getValue 

  eval :: Query ~> H.ComponentDSL State Query (Aff (E eff))
  eval (Init next) = do
    document <- H.fromEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument

    let
      keyboardSource :: H.EventSource (Coproduct (Const Unit) Query) (Aff (E eff))
      keyboardSource =
        H.eventSource (K.onKeyUp document) $ \e -> do
          let info = spy $ K.readKeyboardEvent e
          case info.keyCode of
            -- Left Arrow	
            37 ->  pure $ H.action (right <<< Move Timeline.MoveReverse)
            -- Right Arrow
            39 -> pure $ H.action (right <<< Move Timeline.MoveForward)
            -- Left Arrow	
            33 ->  pure $ H.action (right <<< Move Timeline.MoveReverse)
            -- Right Arrow
            34 -> pure $ H.action (right <<< Move Timeline.MoveForward)
            -- Home
            36 -> pure $ H.action (right <<< Move Timeline.MoveBegin)
            -- End
            35 -> pure $ H.action (right <<< Move Timeline.MoveEnd)
            _ -> pure $ left (Const unit)
      animationFrameSource :: H.EventSource Query (Aff (E eff))
      animationFrameSource =
        H.eventSource A.onAnimationFrameDelta $ \dt ->
          pure $ H.action (Tick dt)
    H.subscribe $ ES.catEventSource keyboardSource
    H.subscribe animationFrameSource
    body <- H.fromAff (selectElement "body") <#> (map DOM.htmlElementToElement)
    case body of
      Nothing -> pure unit
      Just b -> H.fromEff $ KaTeX.renderMathInElement b KaTeX.optionsWithDollar
    pure next
  eval (Move target next) = do
    H.modify (Timeline.move target)
    i <- H.gets Timeline.getStepIndex
    H.fromEff (R.setHash ("/" <> (show i)))
    pure next
  eval (Tick dt next) = do
    H.modify (Timeline.tick dt)
    pure next
    
data Routes = RouteSlide Number

routing :: R.Match Routes
routing = RouteSlide <$> (R.lit "" *> R.num)

routeSignal :: forall eff. H.Driver Query eff
            -> Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff) Unit
routeSignal driver = do
  Tuple old new <- R.matchesAff routing
  redirects driver old new

redirects :: forall eff. H.Driver Query eff
          -> Maybe Routes
          -> Routes
          -> Aff (dom :: DOM, avar :: AVAR, err :: EXCEPTION | eff) Unit
redirects driver _ (RouteSlide t) = do
  driver (H.action (Move (Timeline.MoveTo t Timeline.Forward)))

main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- H.runUI ui initialState body
  forkAff $ routeSignal driver