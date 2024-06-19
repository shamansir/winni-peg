module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Data.Maybe (Maybe(..))

import Grammar (Grammar)
import Grammar.AST (AST)


type State =
  { input :: String
  , grammarInput :: String
  , grammar :: Maybe Grammar
  }


type Input =
  { }


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


initialState :: State
initialState =
  { input : ""
  , grammarInput : ""
  , grammar : Nothing
  }


data Action = Increment | Decrement


component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState : const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      , HH.textarea [ HE.onInput \_ -> Decrement, HE.onValueChange \_ -> Increment ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state { input = state.input <> "a" }
    Decrement -> H.modify_ \state -> state { input = state.input <> "b" }