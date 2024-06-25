module App where

import Prelude

import Type.Proxy (Proxy(..))

import Debug as Debug

import Data.Time.Duration (Milliseconds(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)

import Effect.Class (liftEffect)
import Effect.Aff (forkAff, delay) as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console

import Control.Monad.Rec.Class (forever)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import Grammar (Grammar)
import Grammar.Parser (parser) as Grammar
import Grammar.AST (AST)
import Grammar.AST.Parser (parse) as AST

import Parsing (runParser, ParseError) as P
-- import Parsing.String (parseErrorHuman) as P


import Component.Grammar (component) as GrammarCmp
import Component.AST (component) as ASTCmp


refreshInterval :: Milliseconds
refreshInterval = Milliseconds 1000.0


type State =
  { input :: String
  , grammarInput :: String
  , grammar :: Maybe (Either P.ParseError Grammar)
  , ast :: Maybe (AST String)
  , prevInput :: String
  , prevGrammarInput :: String
  , count :: Int
  }


type Slots =
  ( ast :: forall query. H.Slot query Void Int
  , grammar :: forall query. H.Slot query Void Int
  )


_ast = Proxy :: _ "ast"
_grammar = Proxy :: _ "grammar"


initialState :: State
initialState =
  { input : "aaa"
  , grammarInput : "main :- repSep('a',\"\")."
  , grammar : Nothing
  , ast : Nothing
  , prevInput : ""
  , prevGrammarInput : ""
  , count : 0
  }


type InputText = String


type GrammarInputText = String


data Action
  = Initialize
  | Skip
  | UpdateInput InputText
  | UpdateGrammarInput GrammarInputText
  | UpdateGrammar Grammar
  | UpdateAST (AST String)
  | GrammarErrorOccured P.ParseError
  | CompileGrammar GrammarInputText
  | ParseInput InputText
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

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.div_
      -- [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      -- , HH.div_ [ HH.text $ show state ]
      -- , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      [ HH.text "Input"
      , HH.text $ if state.prevInput /= state.input then "*" else "v"
      , HH.textarea [ HP.cols 80, HP.rows 60, HE.onValueInput UpdateInput, HP.value state.input ]
      , HH.text "Grammar"
      , HH.text $ if state.prevGrammarInput /= state.grammarInput  then "*" else "v"
      , HH.text $ show state.count
      , HH.textarea [ HP.cols 80, HP.rows 60, HE.onValueInput UpdateGrammarInput, HP.value state.grammarInput ]
      -- , HH.text state.grammarInput
      -- , HH.text $ show state.grammar
      , case state.grammar of
          Nothing -> HH.text "No grammar"
          Just (Right grammar) -> HH.slot_ _grammar 0 GrammarCmp.component grammar
          Just (Left parseError) -> HH.text $ show parseError -- String.joinWith "\n" $ P.parseErrorHuman state.input parseError 4
      , case state.ast of
          Nothing -> HH.text "No AST"
          Just ast -> HH.slot_ _ast 0 ASTCmp.component ast
      ]

  handleAction = case _ of
    Initialize -> do
      handleAction Tick
      _ <- H.subscribe =<< timer Tick
      pure unit
    Skip -> pure unit
    UpdateInput to ->            H.modify_ \s -> s { input = to }
    UpdateGrammarInput to ->     H.modify_ \s -> s { grammarInput = to }
    UpdateAST ast ->             H.modify_ \s -> s { ast = Just ast }
    UpdateGrammar grammar ->     H.modify_ \s -> s { grammar = Just $ Right grammar }
    GrammarErrorOccured error -> H.modify_ \s -> s { grammar = Just $ Left error }
    CompileGrammar grammarInput -> do
      state <- H.get
      let grammarResult = P.runParser grammarInput Grammar.parser
      liftEffect $ Console.log "update grammar"
      handleAction $ either GrammarErrorOccured UpdateGrammar grammarResult
      handleAction $ ParseInput state.input
    ParseInput input -> do
      state <- H.get
      case state.grammar of
        Just (Right grammar) -> do
          liftEffect $ Console.log "update ast"
          let ast = AST.parse grammar (const "") input
          handleAction $ UpdateAST ast
        _ ->
          pure unit
    Tick -> do
      state <- H.get
      liftEffect $ Console.log "tick"
      if state.grammarInput /= state.prevGrammarInput
      then do
          handleAction $ CompileGrammar state.grammarInput
          H.modify_ \s -> s { prevGrammarInput = state.grammarInput }
      else
        if state.input /= state.prevInput
            then do
              handleAction $ ParseInput state.input
              H.modify_ \s -> s { prevInput = state.input }
        else
          pure unit


timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay refreshInterval
    H.liftEffect $ HS.notify listener val
  pure emitter