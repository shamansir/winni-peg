module Component.AST where

import Prelude

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS


import Grammar.AST (AST)
import Grammar.AST (empty) as AST


data NodeState
    = IsLeaf
    | Expanded
    | Collapsed


type State = AST String


initialState :: State
initialState = AST.empty


type Action = Unit


component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState : const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }
  where

  render :: forall action slots. State -> H.ComponentHTML action slots m
  render ast =
    HH.div_
      [ HH.text "YAY"
      ]

  handleAction = case _ of
    _ -> pure unit