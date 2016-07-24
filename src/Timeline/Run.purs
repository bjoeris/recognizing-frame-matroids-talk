module Timeline.Run where

import Prelude

import Data.Time.Duration (Milliseconds, unMilliseconds)
import Data.Array as Array
import Data.Maybe (Maybe(Just,Nothing), fromMaybe)
import Data.Int as Int
import Data.Maybe.Unsafe (unsafeFromMaybeWith)

import Debug.Trace (traceAny,spy)

import Halogen as H

import Timeline.Build
import Timeline.Tween
import Timeline.Compile

data Direction
  = Forward
  | Reverse

data StepState
  = StepPaused Int
  | StepTrans
    { stepIndex :: Int
    , progress :: Number
    , direction :: Direction
    }

type StateProps b a =
  { compiled :: Compiled b (Tween b a)
  , stepState :: StepState
  }

data State a = State (forall b. StateProps b a)

initialState :: forall a. (forall b. Timeline b (Tween b a)) ->  State a
initialState t = State 
  { compiled: compile t
  , stepState: StepPaused 0
  }

stateProps :: forall b a. State a -> StateProps b a
stateProps (State p) = p

data Movement
  = MoveForward
  | MoveReverse
  | MoveBegin
  | MoveEnd
  | MoveTo Number Direction

clampState :: forall b a. StateProps b a -> StateProps b a
clampState state = {compiled : state.compiled, stepState}
  where
  n = numSteps state.compiled
  stepState = case state.stepState of
    StepPaused i -> StepPaused $ clamp 0 (n-1) i
    StepTrans trans -> 
      if trans.stepIndex < 0
      then StepPaused 0
      else if trans.stepIndex >= n
            then StepPaused (n-1)
            else StepTrans trans


move' :: forall b a. Movement -> StateProps b a -> StateProps b a
move' target state = 
  {compiled: state.compiled, stepState} # clampState
  where
  completedStepIndex = case state.stepState of
    StepPaused i -> i
    StepTrans {direction, stepIndex} -> case direction of
      Forward -> stepIndex
      Reverse -> stepIndex-1
  stepState = case target of
    MoveForward -> StepTrans 
      { stepIndex : completedStepIndex + 1
      , progress : 0.0
      , direction : Forward
      }
    MoveReverse -> StepTrans 
      { stepIndex : completedStepIndex
      , progress : 1.0
      , direction : Reverse
      }
    MoveBegin -> StepPaused 0
    MoveEnd -> StepPaused (numSteps state.compiled - 1)
    MoveTo t dir ->
      let i = Int.floor t
      in if i == completedStepIndex
         then state.stepState
         else case Int.fromNumber t of
            Nothing -> StepTrans
              { stepIndex : i
              , progress : t - (Int.toNumber i)
              , direction : dir
              }
            Just i -> StepPaused i

move :: forall a. Movement -> State a -> State a
move target state = traceAny {target,state} $ \_ -> spy $ State (move' target (stateProps state))

tick' :: forall b a. Milliseconds -> StateProps b a -> StateProps b a
tick' dt state =
  {compiled: state.compiled, stepState} # clampState
  where
  cp = compiledProps (state.compiled)
  stepState = case state.stepState of
    StepPaused i -> state.stepState
    StepTrans trans -> traceAny trans $ \_ ->
      let d = case trans.direction of
            Forward -> 1.0
            Reverse -> -1.0
          duration = trans.stepIndex
            # Array.index cp.steps
            # unsafeFromMaybeWith "error looking up step duration"
            # \step -> step.duration
          speed = d / (unMilliseconds duration)
          progress = trans.progress + speed * (unMilliseconds dt) # clamp 0.0 1.0
      in if progress <= 0.0 && d < 0.0
          then StepPaused (trans.stepIndex - 1)
          else if progress >= 1.0 && d > 0.0
                then StepPaused trans.stepIndex
                else StepTrans $ trans { progress = progress }

tick :: forall a. Milliseconds -> State a -> State a
tick dt state = State (tick' dt (stateProps state))

getValue' :: forall b a. StateProps b a -> a
getValue' {compiled: Compiled {steps,lookupStep,value: Tween evalTween},stepState} =
  evalTween getProgress
  where
    t = case stepState of
      StepPaused i -> Int.toNumber (i+1)
      StepTrans {stepIndex,progress} -> traceAny {stepIndex,progress} $ \_ -> Int.toNumber stepIndex + progress
    getProgress i = (t - (lookupStep i # Int.toNumber)) # clamp 0.0 1.0

getValue :: forall a. State a -> a
getValue state = getValue' (stateProps state)

getStepIndex :: forall a. State a -> Int
getStepIndex (State{stepState}) = case stepState of
  StepPaused i -> i
  StepTrans {progress,stepIndex,direction} -> case direction of 
    Forward -> stepIndex
    Reverse -> max (stepIndex - 1) 0

-- data Query n
--   = Move Movement n
--   | Tick Milliseconds n
--   -- | GetValue (H.ComponentHTML Query -> n)

-- eval :: forall a g. Query ~> H.ComponentDSL (State a) Query g
-- eval (Move target next) = do
--   H.modify (move target)
--   pure next
-- eval (Tick dt next) = do
--   H.modify (tick dt)
--   pure next
-- -- eval (GetValue continue) = do
-- --   value <- H.gets getValue
-- --   pure (continue value)