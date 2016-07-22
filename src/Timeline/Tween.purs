module Timeline.Tween where

import Prelude
import Timeline.Build

import Debug.Trace

newtype Tween b a = Tween ((StepId b -> Number) -> a)

type TTween b a = Timeline b (Tween b a)

evalTween :: forall b a. Tween b a -> (StepId b -> Number) -> a
evalTween (Tween f) = f

instance functorTween :: Functor (Tween b) where
  map f tx = Tween $ \p -> 
    let x = evalTween tx p 
    in f x

instance applyTween :: Apply (Tween b) where
  apply tf tx = Tween $ \p ->
    let x = evalTween tx p
        f = evalTween tf p
    in f x

instance applicativeTween :: Applicative (Tween b) where
  pure x = Tween $ \_ -> x

instance bindTween :: Bind (Tween b) where
  bind tx f = Tween $ \p -> 
    evalTween (f (evalTween tx p)) p

instance monadTween :: Monad (Tween b)

tween :: forall b a. StepId b -> (Number -> a) -> Tween b a
tween i f = Tween (\progress -> f (progress i))

ttween' :: forall b a. StepId b -> (Number -> a) -> Timeline b (Tween b a)
ttween' i f = pure $ tween i f

ttween :: forall b a. (Number -> a) -> Timeline b (Tween b a)
ttween f = do
  i <- newStep
  traceAnyA i
  ttween' i f
