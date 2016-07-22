module Data.Maybe.Unsafe where

import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(),fromMaybe')

unsafeFromMaybeWith :: forall a. String -> Maybe a -> a
unsafeFromMaybeWith msg = fromMaybe' (\_ -> unsafeCrashWith msg)