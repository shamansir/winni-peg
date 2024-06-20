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
import Data.Either (Either(..))

import Grammar (Grammar)
import Grammar.Parser (parser) as Grammar
import Grammar.AST (AST)
import Grammar.AST.Parser (parse) as AST

import Parsing (runParser, ParseError) as P


refreshInterval = Milliseconds 1000.0


type State =
  { input :: String
  , grammarInput :: String
  , grammar :: Maybe (Either P.ParseError Grammar)
  , ast :: Maybe (AST String)
  , inputChanged :: Boolean
  , grammarInputChanged :: Boolean
  }


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


initialState :: State
initialState =
  { input : ""
  , grammarInput : ""
  , grammar : Nothing
  , ast : Nothing
  , inputChanged : false
  , grammarInputChanged : false
  }


type InputText = String


type GrammarInputText = String


data Action
  = Initialize
  | Skip
  | UpdateInput InputText
  | UpdateGrammarInput GrammarInputText
  -- | UpdateGrammar Grammar
  -- | UpdateAST (AST String)
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
      -- [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      -- , HH.div_ [ HH.text $ show state ]
      -- , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      [ HH.text "Input"
      , HH.textarea [ HP.cols 80, HP.rows 60, HE.onValueInput UpdateInput ] -- \evt -> IE.fromEvent evt <#> IE.data_ # fromMaybe "<>" # ChangeInput ]
      , HH.text "Grammar"
      , HH.textarea [ HP.cols 80, HP.rows 60, HE.onValueInput UpdateGrammarInput ] -- \evt -> IE.fromEvent evt <#> IE.data_ # fromMaybe "<>" # ChangeInput ]
      ]

  handleAction = case _ of
    Initialize -> do
      _ <- H.subscribe =<< timer Tick
      pure unit
    Skip -> pure unit
    UpdateInput to -> H.modify_ \s -> s { input = to, inputChanged = true }
    UpdateGrammarInput to -> H.modify_ $ \s -> s { grammarInput = to, grammarInputChanged = true }
    -- UpdateGrammar grammar -> H.modify_ \s -> s { grammar = Just $ Right grammar }
    -- UpdateAST ast -> H.modify_ \s -> s { ast = Just ast }
    Tick -> do
      state <- H.get
      if state.grammarInputChanged
        then
          let grammarResult = P.runParser state.grammarInput Grammar.parser
          in H.modify_ \s -> s { grammarInputChanged = false, grammar = Just grammarResult }
        else pure unit
      if state.inputChanged
        then do
          state' <- H.get -- to get new one if grammar was changed
          case state'.grammar of
            Just (Right grammar) -> do
              let ast = AST.parse grammar (const "") state'.input
              H.modify_ \s -> s { ast = Just ast, inputChanged = false }
            _ -> pure unit
        else pure unit


timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay refreshInterval
    H.liftEffect $ HS.notify listener val
  pure emitter