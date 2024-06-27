module Component.AST where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length) as Array
import Data.String (length) as String
import Data.Tuple.Nested ((/\), type (/\))

import Effect.Console (log) as Console
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import Web.HTML.Common (ClassName(..))

import Yoga.Tree (Tree, showTree)
import Yoga.Tree.Extended as Tree
import Yoga.Tree.Extended (update) as Tree
import Yoga.Tree.Extended.Path (Path) as Tree
import Yoga.Tree.Extended.Path (fill, with) as Path

import Debug as Debug


import Grammar (Rule(..), WhichChar(..), toRepr) as G
import Grammar.AST (AST, ASTNode)
import Grammar.AST (Attempt(..), At(..), Cell) as AST
import Grammar.AST (empty, root, mapBy, match) as AST


data NodeExpand
    = IsLeaf
    | Expanded
    | Collapsed


type UIState =
  { expand :: NodeExpand
  , path :: Tree.Path
  }


type CellWithState =
  { state :: UIState
  , cell :: AST.Cell String
  }


type Input = AST String


type State = Tree CellWithState


load :: Input -> State
load = AST.root >>> Path.fill >>> map \(path /\ cell) -> { cell, state : { expand : Expanded, path } }


toggleExpand :: CellWithState -> CellWithState
toggleExpand { state, cell } =
  { cell, state : state
    { expand = case state.expand of
      IsLeaf -> IsLeaf
      Expanded -> Collapsed
      Collapsed -> Expanded
    }
  }


data Action
  = Receive Input
  | Toggle Tree.Path


component :: forall query output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState : load
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render tree =
    HH.div
        [ HP.class_ $ ClassName "ast" ]
        [ {- HH.div [ HP.class_ $ ClassName "textual" ] [ HH.text $ show ast ]
        , -} renderNode tree
        ]

  renderNode :: forall slots. Tree CellWithState -> H.ComponentHTML Action slots m
  renderNode node =
    let
        knot = Tree.value node
        classNameByRule = _.cell >>> _.rule >>> case _ of
          G.Sequence _ -> "x-sequence"
          G.Choice _ -> "x-choice"
          G.Ref _ _ -> "x-ref"
          G.Text _ -> "x-text"
          G.RepSep _ _ -> "x-repsep"
          G.Char G.Any -> "x-any-char"
          G.Char (G.Range _ _) -> "x-char-rng"
          G.Char (G.Not _) -> "x-not-char"
          G.Char (G.Single _) -> "x-char"
          G.Placeholder -> "x-placeholder"
          G.None -> "x-none"
        classNameByResult = _.cell >>> _.result >>> case _ of
          AST.Match _ _ -> "x-match"
          AST.Fail _ _ -> "x-fail"
        ruleLabel = _.cell >>> _.rule >>> case _ of
          G.Sequence _ -> "Sequence"
          G.Choice _ -> "Choice"
          G.Ref _ _ -> "Ref"
          G.Text _ -> "Text"
          G.RepSep _ _ -> "Repeat/Separate"
          G.Char G.Any -> "Any Char"
          G.Char (G.Range _ _) -> "Char Range"
          G.Char (G.Not _) -> "Not Char"
          G.Char (G.Single _) -> "Single Char"
          G.Placeholder -> "Placeholder"
          G.None -> "-"
        ruleValue = _.cell >>> _.rule >>> case _ of
          G.Sequence seq -> Just $ "(" <> show (Array.length seq) <> ")"
          G.Choice options -> Just $ "[" <> show (Array.length options) <> "]"
          G.Ref mbCapture ruleName -> Just $ "<" <> fromMaybe ruleName mbCapture <> ">"
          G.Text text -> Just text
          G.RepSep _ _ -> Nothing
          G.Char G.Any -> Nothing
          G.Char (G.Range from to) -> Just $ show from <> "-" <> show to
          G.Char (G.Not char) -> Just $ G.toRepr char
          G.Char (G.Single char) -> Just $ G.toRepr char
          G.Placeholder -> Nothing
          G.None -> Nothing
        -- atLabel = case _ of
        --   AST.Main -> "x"
        --   _ -> "a"
        attemptPos = _.cell >>> _.result >>> case _ of
          AST.Match range _ -> show range.start <> ":" <> show range.end
          AST.Fail pos _ -> show pos
        attemptValue = _.cell >>> _.result >>> case _ of
          AST.Match _ a -> a
          AST.Fail _ error -> show error
    in
        HH.div
            [ HP.classes [ ClassName "node", ClassName $ classNameByRule knot, ClassName $ classNameByResult knot ]
            ]
            [ HH.div
              [ HP.class_ $ ClassName "row"
              , HE.onClick $ const $ Toggle knot.state.path
              ]
              [ HH.span
                  [ HP.class_ $ ClassName "label" ]
                  [ HH.text $ ruleLabel knot ]
              , case ruleValue knot of
                Just value ->
                  HH.span
                    [ HP.class_ $ ClassName "value" ]
                    [ HH.text $ if String.length value > 0 then value else "\"\"" ]
                Nothing -> HH.text ""
              , HH.span
                  [ HP.class_ $ ClassName "attempt-pos" ]
                  [ HH.text $ attemptPos knot ]
              , HH.span
                  [ HP.class_ $ ClassName "attempt-value" ]
                  [ HH.text $ attemptValue knot ]
              -- , HH.span
              --     [ HP.class_ $ ClassName "path" ]
              --     [ HH.text $ show knot.state.path ]
              ]
            , case knot.state.expand of
              IsLeaf -> HH.text ""
              Collapsed -> HH.text ""
              Expanded ->
                HH.div
                  [ HP.class_ $ ClassName "children" ]
                  $ renderNode <$> Tree.children node
            ]

  handleAction = case _ of
    Receive ast -> H.put $ load ast
    Toggle path -> do
      liftEffect $ Console.log $ show path
      H.modify_ $ Path.with path $ Tree.update toggleExpand