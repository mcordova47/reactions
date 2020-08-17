module Feather
  ( externalLink
  ) where

import Elmish (createElement')
import Elmish.React.Import (ImportedReactComponent, ImportedReactComponentConstructor, EmptyProps)

type OptProps r =
  ( color :: String
  , size :: Int
  | r
  )

externalLink :: ImportedReactComponentConstructor EmptyProps OptProps
externalLink =
  createElement' externalLink_

foreign import externalLink_ :: ImportedReactComponent
