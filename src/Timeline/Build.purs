module Timeline.Build where

import Prelude
import Data.Monoid (mempty)
import Data.Array as Array
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Tuple (Tuple(Tuple))
import Data.Time.Duration (Milliseconds, unMilliseconds, Seconds(Seconds), class Duration, fromDuration)
import Halogen as H

newtype StepId b = StepId Int

instance eqStepId :: Eq (StepId b) where
  eq (StepId x) (StepId y) = x == y

data StepPosition b
  = StepBegin
  | StepEnd
  | StepBefore (StepId b)
  | StepAfter (StepId b)

type Step b = 
  { id :: StepId b
  , duration :: Milliseconds
  , position :: StepPosition b
  }

data Gen a = Gen a (Unit -> Gen a)

initIdGen :: forall b. Gen (StepId b)
initIdGen = go 0
  where
  go i = Gen (StepId i) (\_ -> go (i+1)) 

type TimelineProps b a = 
  { steps :: Array (Step b)
  , idGen :: Gen (StepId b)
  , value :: a
  }

newtype Timeline b a = Timeline
  (Gen (StepId b) -> TimelineProps b a)

timelineProps :: forall b a. Timeline b a -> Gen (StepId b) -> TimelineProps b a
timelineProps (Timeline f) = f 

instance functorTimeline :: Functor (Timeline b) where
  map f tx = Timeline $ \g -> 
    let x = timelineProps tx g 
    in x { value = f x.value }

instance applyTimeline :: Apply (Timeline b) where
  apply tf tx = Timeline $ \g ->
    let f = timelineProps tf g
        x = timelineProps tx f.idGen
    in { steps : f.steps <> x.steps 
       , value : f.value x.value
       , idGen : x.idGen
       }

instance applicativeTimeline :: Applicative (Timeline b) where
  pure x = Timeline $ \g -> { steps : mempty, value : x, idGen : g }

instance bindTimeline :: Bind (Timeline b) where
  bind tx f = Timeline $ \g -> 
    let x = timelineProps tx g
        y = timelineProps (f x.value) x.idGen
    in y { steps = x.steps <> y.steps }

instance monadTimeline :: Monad (Timeline b)

newStep' :: forall d b. Duration d => StepPosition b -> d -> Timeline b (StepId b)
newStep' position duration = Timeline $ \g -> 
  case g of
    Gen id g' ->
      let steps = Array.singleton { id, duration : fromDuration duration, position }
      in { steps, value: id, idGen: g' unit }

defaultDuration :: Seconds
defaultDuration = Seconds 0.1

newStep :: forall d b. Timeline b (StepId b)
newStep = newStep' StepEnd defaultDuration

