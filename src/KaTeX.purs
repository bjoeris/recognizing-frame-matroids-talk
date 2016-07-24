module KaTeX where

import Prelude (Unit)

import Control.Monad.Eff (Eff)

import DOM (DOM())
import DOM.Node.Types (Element)

import Data.Array as Array

type Options = 
  { delimiters :: Array
    { left :: String
    , right :: String
    , display :: Boolean
    }
  , ignoredTags :: Array String
  }

options :: Options
options = 
  { delimiters:
    [ {left: "$$", right: "$$", display: true}
    , {left: "\\[", right: "\\]", display: true}
    , {left: "\\(", right: "\\)", display: false}
    ]
  , ignoredTags: ["script", "noscript", "style", "textarea", "pre", "code"]
  }

optionsWithDollar :: Options
optionsWithDollar = options
  { delimiters = Array.snoc options.delimiters
    {left: "$", right: "$", display: false}
  }

foreign import renderMathInElement :: forall eff. Element -> Options -> Eff (dom :: DOM | eff) Unit