module Component.Grammar where


import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton) as String

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import Web.HTML.Common (ClassName(..))


import Yoga.Tree (Tree, showTree)
import Yoga.Tree.Extended as Tree
-- import Yoga.Tree.Zipper as Tree


import Grammar (Grammar)
import Grammar (empty, toTree) as Grammar
import Grammar (RuleKnot(..), WhichChar(..), toChar) as G


data NodeState
    = IsLeaf
    | Expanded
    | Collapsed


type Input = Grammar


type State = Grammar


initialState :: Input -> State
initialState = identity


data Action =
    Receive Input


component :: forall query output m. MonadAff m => H.Component query Input output m
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
  render grammar =
    HH.div
        [ HP.class_ $ ClassName "grammar" ]
        [ {- HH.div [ HP.class_ $ ClassName "textual" ] [ HH.text $ showTree $ Grammar.toTree grammar ]
        -- HH.div [ HP.class_ $ ClassName "textual" ] [ HH.text $ show grammar ]
        , -} renderNode $ Grammar.toTree grammar
        ]


  renderNode :: forall action slots. Tree G.RuleKnot -> H.ComponentHTML action slots m
  renderNode node =
    let
        knot = Tree.value node
        knotClassName = case _ of
            G.KRoot -> "k-root"
            G.KMain -> "k-main"
            G.KRuleDef _ -> "k-rule-def"
            G.KSequence -> "k-sequence"
            G.KChoice -> "k-choice"
            G.KRepSep -> "k-repsep"
            G.KRef _ _ -> "k-ref"
            G.KText _ -> "k-text"
            G.KChar G.Any -> "k-char-any"
            G.KChar (G.Range _ _) -> "k-char-rng"
            G.KChar (G.Not _) -> "k-char-not"
            G.KChar (G.Single _) -> "k-char-single"
            G.KPlaceholder -> "k-placeholeder"
            G.KNone -> "k-none"
        knotLabel = show
    in
        HH.div
            [ HP.classes [ ClassName "node", ClassName $ knotClassName knot ]
            ]
            [ HH.div
                [ HP.class_ $ ClassName "label" ]
                [ HH.text $ knotLabel knot ]
            , HH.div
                [ HP.class_ $ ClassName "children" ]
                $ renderNode <$> Tree.children node
            ]


  handleAction = case _ of
    Receive grammar -> H.put grammar
    _ -> pure unit
