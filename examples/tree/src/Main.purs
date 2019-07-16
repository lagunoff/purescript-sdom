module Main where

import Prelude

import Control.Lazy (defer)
import Data.Array (intercalate, mapWithIndex)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Lens (Lens', lens')
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import SDOM (ArrayChannel(..), SDOM, array, attach, interpretChannel, text, text_)
import SDOM.Attributes as A
import SDOM.Elements as E
import SDOM.Events as Events
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

data Tree a = Tree a (Array (Tree a))

derive instance functorTree :: Functor Tree

data TreeChannel i channel
  = ParentChannel channel
  | ModifyRoot (Tree i -> Tree i)

type LeafModel a = { path :: Array Int, here :: a }

buildTree :: Int -> Tree Boolean
buildTree 0 = Tree false []
buildTree n = Tree false [ t, t ]
  where t = buildTree (n - 1)

tree
  :: forall channel model
   . (model -> Boolean)
  -> SDOM (TreeChannel model channel) (LeafModel model) (LeafModel model)
  -> SDOM channel (Tree model) (Tree model)
tree getExpanded leaf = interpretChannel (map treeChannel) (_emptyPath $ go leaf)
  where
    go :: forall channel'
        . SDOM channel' (LeafModel model) (LeafModel model)
       -> SDOM channel' (LeafModel (Tree model)) (LeafModel (Tree model))
    go l = defer \_ ->
      E.li_
        [ _inHere l
        , E.span [ A.className \{ here: Tree model _ } -> if getExpanded model then "" else "collapsed" ] []
            [ _inPath
              $ array "ul"
              $ go
              $ interpretChannel (map (lmap Parent <<< Left))
                l
            ]
        ]

    treeChannel
      :: TreeChannel model channel
      -> Either channel (Tree model -> Tree model)
    treeChannel (ParentChannel c) = Left c
    treeChannel (ModifyRoot f) = Right f

    _inHere 
      :: forall a
       . Lens' (LeafModel (Tree a)) (LeafModel a)
    _inHere = lens' (\{ path, here: Tree lf bs } -> Tuple { path, here: lf } (({ path, here: _}) <<< (_ `Tree` bs) <<< _.here))

    _inPath
      :: forall a
       . Lens' (LeafModel (Tree a)) (Array (LeafModel (Tree a)))
    _inPath = lens' \{ path, here: Tree lf bs } -> Tuple (mapWithIndex (\idx here -> { path: path <> [idx], here }) bs) (\bs' -> { path, here: Tree lf (map _.here bs') })

    _emptyPath
      :: forall a
       . Lens' a (LeafModel a)
    _emptyPath = lens' \here -> Tuple { path: [], here } _.here


node
  :: forall channel
   . SDOM
       (TreeChannel Boolean channel)
       (LeafModel Boolean)
       (LeafModel Boolean)
node = E.span_ [ E.button
                   []
                   [ Events.click \_ b -> Right (\{ path, here } -> { here: not here, path }) ]
                   [ text \{ here } -> if here then "-" else "+" ]
               , text \{ path } -> intercalate " / " $ ["Root"] <> map show path
               ]

app
  :: forall channel
   . SDOM channel (Tree Boolean) (Tree Boolean)
app =
  E.div_
    [ E.h1_ [ text_ "Tree" ]
    , E.div [ A.id \_ -> "tree" ] [] [ tree identity node ]
    ]

main :: Effect Unit
main = do
  document <- map toNonElementParentNode (window >>= document)
  container <- getElementById "container" document
  case container of
    Just el -> void do
      attach el (buildTree 4) app
    Nothing -> throw "No 'container' node!"
