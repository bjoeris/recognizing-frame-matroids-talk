module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM

import Halogen as H
import Halogen.HTML.Core (Prop(), prop, propName, attrName)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I(), InteractiveEvents, GlobalProperties)
import Halogen.HTML.Properties.Tweened (ttweenProp)
import Halogen.Query.EventSource as ES
import Halogen.Util (awaitBody, runHalogenAff)

import Keyboard as K
import RequestAnimationFrame as A

import Data.Char as CH
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Maybe (Maybe(Just,Nothing))
import Data.Time.Duration (Milliseconds, Seconds(Seconds))
import Data.Traversable (sequence)

import Timeline.Build as Timeline
import Timeline.Tween as Timeline
import Timeline.Compile as Timeline
import Timeline.Run as Timeline
import Timeline.Tween (TTween, ttween')
-- import Timeline.Dom (text,div,tweenStyle,tweenOpacity)

import Halogen.HTML.Styles.Indexed as I
import Halogen.HTML.Elements.Tweened (div, text)
import Halogen.HTML.Styles.Tweened

import Pokeball

import D3.Geo

import Debug.Trace

data Query a 
  = Init a
  | Move Timeline.Movement a
  | Tick Milliseconds a

type State = Timeline.State (H.ComponentHTML Query)

timeline :: forall b. TTween b (H.ComponentHTML Query)
timeline = do
  i0 <- Timeline.newStep
  --opacityTween <- tween (\t -> "opacity: " <> show t)
  div [backgroundColor "lightblue"] 
    [ div [ttweenProp I.opacity (\t -> t)]
        [ text "hello" ]
    , div [] [ pokeball ]
    ]
  -- styleTween <- style <$> opacityTween
  -- attributes <- sequence $ sequence [styleTween] 
  -- pure $ (HH.div <$> attributes
  --   <*> pure [ HH.text "hello" ])
    
initialState :: State
initialState = spy $ Timeline.initialState timeline

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
          let info = K.readKeyboardEvent e
          if info.keyCode == 37 -- Left Arrow
            then pure $ H.action (right <<< Move Timeline.MoveReverse)
            else if info.keyCode == 39 -- Right Arrow
                  then pure $ H.action (right <<< Move Timeline.MoveForward)
                  else pure $ left (Const unit)
      animationFrameSource :: H.EventSource Query (Aff (E eff))
      animationFrameSource =
        H.eventSource A.onAnimationFrameDelta $ \dt ->
          pure $ H.action (Tick dt)
    H.subscribe $ ES.catEventSource keyboardSource
    H.subscribe animationFrameSource
    pure next
  eval (Move target next) = do
    H.modify (Timeline.move target)
    pure next
  eval (Tick dt next) = do
    H.modify (Timeline.tick dt)
    pure next

-- data Query a = ToggleState a

-- type State = { on :: Boolean }

-- initialState :: State
-- initialState = { on: false }

-- ui :: forall g. H.Component State Query g
-- ui = H.component { render, eval }
--   where
--   render :: State -> H.ComponentHTML Query
--   render state =
--     HH.div [style "background: red"]
--       [ HH.h1_
--           [ HH.text "Hello world!2" ]
--       , HH.p_
--           [ HH.text "Why not toggle this button:" ]
--       , HH.button
--           [ HE.onClick (HE.input_ ToggleState) ]
--           [ HH.text
--               if not state.on
--               then "Don't push me"
--               else "I said don't push me!"
--           ]
--       ]

--   eval :: Query ~> H.ComponentDSL State Query g
--   eval (ToggleState next) = do
--     H.modify (\state -> { on: not state.on })
--     pure next

main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body
