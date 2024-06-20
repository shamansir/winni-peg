module Main where

import Prelude

import Effect (Effect)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (forkAff, delay) as Aff
import Effect.Aff.Class (class MonadAff)

import Control.Monad.Rec.Class (forever)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)

import Web.UIEvent.InputEvent as IE

import Data.Maybe (Maybe(..), maybe, fromMaybe)

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


data Action
  = Initialize
  | ChangeInput String
  | Increment
  | Decrement
  | Tick


component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState : const initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
    }
  where

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      , HH.textarea [ HE.onValueInput ChangeInput ] -- \evt -> IE.fromEvent evt <#> IE.data_ # fromMaybe "<>" # ChangeInput ]
      ]

  handleAction = case _ of
    Initialize -> do
      _ <- H.subscribe =<< timer Tick
      pure unit
    ChangeInput to -> H.modify_ \state -> state { input = to }
    Increment -> H.modify_ \state -> state { input = state.input <> "a" }
    Decrement -> H.modify_ \state -> state { input = state.input <> "b" }
    Tick -> H.modify_ \state -> state { input = state.input <> "1000" }


timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter