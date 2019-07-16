module Main where

import Prelude

import Data.Array (deleteAt, filter, length)
import Data.Either (Either(..))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Exception (throw)
import SDOM (ArrayChannel(..), SDOM, attach, array, text, text_)
import SDOM.Components (textbox, checkbox)
import SDOM.Elements as E
import SDOM.Events as Events
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Task =
  { index :: Int
  , description :: String
  , completed :: Boolean
  }

emptyTask :: Task
emptyTask =
  { index: -1
  , description: ""
  , completed: false
  }

task
  :: forall channel
   . SDOM
       (ArrayChannel Task channel)
       Task
       Task
task = E.span_
  [ checkbox
      (\{ index } -> "task-" <> show index)
      _.completed
      (_ { completed = _ })
  , prop (SProxy :: SProxy "description") textbox
  , E.button
      []
      [ Events.click \{ index } _ -> Left (Here (fromMaybe <*> deleteAt index)) ]
      [ text_ "✕" ]
  ]

type TaskList =
  { tasks :: Array Task
  }

taskList
  :: forall channel
   . SDOM channel TaskList TaskList
taskList = dimap _.tasks { tasks: _ } $
    E.div_
      [ E.h1_ [ text_ "Task List" ]
      , E.button
          []
          [ Events.click \_ _ -> pure \xs -> xs <> [emptyTask { index = length xs }] ]
          [ text_ "＋ New Task" ]
      , array "ol" (E.li_ [ task ])
      , E.p_ [ text summaryLabel ]
      ]
  where
    summaryLabel =
      filter _.completed
      >>> length
      >>> show
      >>> (_ <> " tasks completed.")

main :: Effect Unit
main = do
  document <- map toNonElementParentNode (window >>= document)
  container <- getElementById "container" document
  case container of
    Just el -> void do
      attach el { tasks: [] } taskList
    Nothing -> throw "No 'container' node!"
