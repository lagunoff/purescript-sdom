module Main where

import Prelude

import Data.Array (filter, length)
import Data.Either (Either(..))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Exception (throw)
import SDOM (ArrayChannel(..), Gui, SDOM, attach, array, text, text_)
import SDOM.Components (textbox, checkbox)
import SDOM.Elements as E
import SDOM.Events as Events
import SDOM.Attributes as A
import SDOM.GuiEvent as GuiEvent
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.DOM (Element)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Unsafe.Coerce (unsafeCoerce)

type Task =
  { id :: Int
  , description :: String
  , completed :: Boolean
  }

emptyTask :: Task
emptyTask =
  { id: -1
  , description: ""
  , completed: false
  }

task
  :: forall channel
   . Gui Element (ArrayChannel Task channel) Task Task
task = E.span_
  [ checkbox
      (\{ id } -> "task-" <> show id)
      _.completed
      (_ { completed = _ })
  , prop (SProxy :: SProxy "description") textbox
  , E.button
      [ Events.click \{ id } _ -> GuiEvent.EventChan (Here (filter (_.id >>> (_ /= id))))]
      [ text_ "✕" ]
  ]


type TaskList =
  { tasks :: Array Task
  }

taskList
  :: forall channel
   . Gui Element channel TaskList TaskList
taskList = dimap _.tasks { tasks: _ } $
    E.div_
      [ E.h1_ [ text_ "Task List" ]
      , E.button
          [ Events.click \_ _ -> GuiEvent.EventFn \xs -> xs <> [emptyTask { id = length xs }] ]
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
