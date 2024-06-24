module Component.AST where

import Prelude

import Effect.Aff.Class (class MonadAff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS

import Web.HTML.Common (ClassName(..))

import Yoga.Tree (Tree, showTree)
import Yoga.Tree.Extended as Tree

import Grammar.AST (AST, ASTNode)
import Grammar.AST (empty, root) as AST


data NodeState
    = IsLeaf
    | Expanded
    | Collapsed


type Input = AST String


type State = AST String


initialState :: Input -> State
initialState = identity


type Action = Unit


component :: forall query input output m. MonadAff m => H.Component query Input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
    }
  where

  render :: forall action slots. State -> H.ComponentHTML action slots m
  render ast =
    HH.div
        [ HP.class_ $ ClassName "ast" ]
        [ HH.text $ show ast
        -- , HH.text $ showTree $ AST.root ast
        , renderNode $ AST.root ast
        ]

  renderNode :: forall action slots. ASTNode String -> H.ComponentHTML action slots m
  renderNode node =
    let
        knot = Tree.value node
        knotClassName = _.rule >>> case _ of
            _ -> "k-x"
        knotLabel = _.rule >>> show
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
    _ -> pure unit