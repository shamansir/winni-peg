module Component.AST where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length) as Array
import Data.String (length) as String

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import Web.HTML.Common (ClassName(..))

import Yoga.Tree (Tree, showTree)
import Yoga.Tree.Extended as Tree


import Grammar (Rule(..), WhichChar(..), toRepr) as G
import Grammar.AST (AST, ASTNode)
import Grammar.AST (Attempt(..), At(..)) as AST
import Grammar.AST (empty, root) as AST


data NodeExpand
    = IsLeaf
    | Expanded
    | Collapsed { total :: Int, direct :: Int }


type NodeState = String


type Input = AST NodeState


type State = AST NodeState


initialState :: Input -> State
initialState = identity


data Action =
  Receive Input


component :: forall query input output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
    }
  where

  render :: forall action slots. State -> H.ComponentHTML action slots m
  render ast =
    HH.div
        [ HP.class_ $ ClassName "ast" ]
        [ {- HH.div [ HP.class_ $ ClassName "textual" ] [ HH.text $ show ast ]
        , -} renderNode $ AST.root ast
        ]

  renderNode :: forall action slots. ASTNode NodeState -> H.ComponentHTML action slots m
  renderNode node =
    let
        knot = Tree.value node
        classNameByRule = _.rule >>> case _ of
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
        classNameByResult = _.result >>> case _ of
          AST.Match _ _ -> "x-match"
          AST.Fail _ _ -> "x-fail"
        ruleLabel = case _ of
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
        ruleValue = case _ of
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
        attemptPos = case _ of
          AST.Match range _ -> show range.start <> ":" <> show range.end
          AST.Fail pos _ -> show pos
        attemptValue = case _ of
          AST.Match _ a -> a
          AST.Fail _ error -> show error
    in
        HH.div
            [ HP.classes [ ClassName "node", ClassName $ classNameByRule knot, ClassName $ classNameByResult knot ]
            ]
            [ HH.div
              [ HP.class_ $ ClassName "row" ]
              [ HH.span
                  [ HP.class_ $ ClassName "label" ]
                  [ HH.text $ ruleLabel knot.rule ]
              , case ruleValue knot.rule of
                Just value ->
                  HH.span
                    [ HP.class_ $ ClassName "value" ]
                    [ HH.text $ if String.length value > 0 then value else "\"\"" ]
                Nothing -> HH.text ""
              , HH.span
                  [ HP.class_ $ ClassName "attempt-pos" ]
                  [ HH.text $ attemptPos knot.result ]
              , HH.span
                  [ HP.class_ $ ClassName "attempt-value" ]
                  [ HH.text $ attemptValue knot.result ]
              ]
            , HH.div
                  [ HP.class_ $ ClassName "children" ]
                  $ renderNode <$> Tree.children node
            ]

  handleAction = case _ of
    Receive ast -> H.put ast
    _ -> pure unit