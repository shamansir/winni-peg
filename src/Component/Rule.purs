module Component.Rule where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS


import Grammar (Rule(..), RuleKnot(..), WhichChar(..), toChar) as G


component :: forall action slots m. G.Rule -> H.ComponentHTML action slots m
component _ =
    HH.div [] []