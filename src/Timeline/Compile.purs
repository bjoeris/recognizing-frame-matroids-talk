module Timeline.Compile where

import Prelude
import Data.Maybe (Maybe(Just,Nothing),fromMaybe)
import Data.Monoid (mempty)
import Data.Array as Array
import Data.Maybe.Unsafe (unsafeFromMaybeWith)
import Data.Foldable (foldl)

import Timeline.Build
import Timeline.Tween

import Debug.Trace

type CompiledProps b a =
  { steps :: Array (Step b)
  , lookupStep :: StepId b -> Int
  , value :: a
  }

data Compiled b a = Compiled (CompiledProps b a)

compiledProps :: forall b a. Compiled b a -> CompiledProps b a
compiledProps (Compiled props) = props

compile :: forall b a. Timeline b a -> Compiled b a
compile tt = Compiled
  { steps, lookupStep, value }
  where
    value = t.value
    steps = foldl insertStep (Just mempty) t.steps 
      # unsafeFromMaybeWith "error compiling timeline"
    lookupStep (StepId i) = Array.index lookupArr i 
      # unsafeFromMaybeWith "error looking up step"
    t = timelineProps tt initIdGen
    insertStep :: Maybe (Array (Step b)) -> Step b -> Maybe (Array (Step b))
    insertStep prevStepsM step = case step.position of
      StepBegin -> Array.cons step <$> prevStepsM
      StepEnd -> Array.snoc <$> prevStepsM <*> pure step
      StepBefore id -> do
        prevSteps <- prevStepsM
        i <- Array.findIndex (\step -> step.id == id) prevSteps
        Array.insertAt i step prevSteps
      StepAfter id -> do
        prevSteps <- prevStepsM
        i <- Array.findLastIndex (\step -> step.id == id) prevSteps
        Array.insertAt (i+1) step prevSteps
    n = Array.length steps
    slowLookup i = Array.findIndex (\step -> step.id == StepId i) steps
      # unsafeFromMaybeWith "error constructing step lookup table"
    lookupArr = map slowLookup (Array.range 0 (n-1))

numSteps :: forall b a. Compiled b a -> Int
numSteps (Compiled {steps}) = Array.length steps