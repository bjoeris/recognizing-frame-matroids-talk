-- | This module exposes a polyfilled `requestAnimationFrame` function.
module RequestAnimationFrame
  ( requestAnimationFrame
  , requestAnimationFrame_
  , onAnimationFrame
  , onAnimationFrameDelta
  ) where

import Prelude

import Control.Monad.Eff

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (Window())

import Data.Time.Duration (Milliseconds(Milliseconds), class Duration, toDuration)

-- | Request the specified action be called on the next animation frame, specifying the `Window` object.
foreign import requestAnimationFrameImpl :: forall a eff. Window -> (Number -> Eff (dom :: DOM | eff) a) -> Eff (dom :: DOM | eff) Unit

requestAnimationFrame_ :: forall d a eff. Duration d => Window -> (d -> Eff (dom :: DOM | eff) a) -> Eff (dom :: DOM | eff) Unit
requestAnimationFrame_ window action =
  requestAnimationFrameImpl window $ Milliseconds >>> toDuration >>> action

-- | Request the specified action be called on the next animation frame.
requestAnimationFrame :: forall d a eff. Duration d => (d -> Eff (dom :: DOM | eff) a) -> Eff (dom :: DOM | eff) Unit
requestAnimationFrame action = do
  w <- window
  requestAnimationFrame_ w action

onAnimationFrame :: 
  forall d eff. Duration d =>
  (d -> Eff (dom :: DOM | eff) Unit)
  -> Eff (dom :: DOM | eff) Unit
onAnimationFrame fn = 
  requestAnimationFrame fn'
  where
  fn' t = do
    fn t
    requestAnimationFrame fn'

onAnimationFrameDelta :: 
  forall d eff. Duration d =>
  (d -> Eff (dom :: DOM | eff) Unit)
  -> Eff (dom :: DOM | eff) Unit
onAnimationFrameDelta fn = do
  requestAnimationFrame fn0
  where
  fn0 t =
    requestAnimationFrame (fn' t)
  fn' tPrev t = do
    fn (toDuration (t - tPrev))
    requestAnimationFrame (fn' t)
  