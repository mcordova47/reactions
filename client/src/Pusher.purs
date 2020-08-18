module Pusher
  ( subscribe
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Foreign (Foreign)

subscribe :: String -> (Foreign -> Effect Unit) -> Effect Unit
subscribe =
  runEffectFn2 subscribe_

foreign import subscribe_ :: EffectFn2 String (Foreign -> Effect Unit) Unit
