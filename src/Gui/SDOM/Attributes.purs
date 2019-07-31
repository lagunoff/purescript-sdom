module Gui.SDOM.Attributes
  ( for
  , id
  , name
  , className
  , type_
  , value
  , checked
  , disabled
  ) where

import Prelude

import Gui.SDOM (Attr, unsafeAttr)
import Unsafe.Coerce (unsafeCoerce)
import FRP.Event (subscribe)
import Web.DOM.Element (removeAttribute, setAttribute)
import Web.HTML.HTMLInputElement (setChecked, setDisabled, setValue)

attr :: forall model msg. String -> (model -> String) -> Attr model msg
attr attrName f =
  unsafeAttr \{ ask, updates } e -> do
    let update v = setAttribute attrName v e
    unsubscribe <- updates `subscribe` \{ old, new } -> do
      let oldValue = f old
          newValue = f new
      when (oldValue /= newValue) (update newValue)
    ask >>= update <<< f
    pure unsubscribe

for :: forall model msg. (model -> String) -> Attr model msg
for = attr "for"

id :: forall model msg. (model -> String) -> Attr model msg
id = attr "id"

className :: forall model msg. (model -> String) -> Attr model msg
className = attr "class"

name :: forall model msg. (model -> String) -> Attr model msg
name = attr "name"

type_ :: forall model msg. (model -> String) -> Attr model msg
type_ = attr "type"

value :: forall model msg. (model -> String) -> Attr model msg
value f =
  unsafeAttr \{ ask, updates } tn -> do
    let update s = do
          setValue s (unsafeCoerce tn)
          setAttribute "value" s tn
    unsubscribe <- updates `subscribe` \{ old, new } -> do
      let oldValue = f old
          newValue = f new
      when (oldValue /= newValue) (update newValue)
    ask >>= update <<< f
    pure unsubscribe
   
checked :: forall model msg. (model -> Boolean) -> Attr model msg
checked f =
  unsafeAttr \{ ask, updates } e -> do
    let update b = do
          setChecked b (unsafeCoerce e)
          if b then setAttribute "checked" "checked" e
               else removeAttribute "checked" e
    unsubscribe <- updates `subscribe` \{ old, new } -> do
      update (f new)
    ask >>= update <<< f
    pure unsubscribe

disabled :: forall model msg. (model -> Boolean) -> Attr model msg
disabled f =
  unsafeAttr \{ ask, updates } e -> do
    let update b = do
          setDisabled b (unsafeCoerce e)
          if b then setAttribute "disabled" "disabled" e
               else removeAttribute "disabled" e
    unsubscribe <- updates `subscribe` \{ old, new } -> do
      let oldValue = f old
          newValue = f new
      when (oldValue /= newValue) (update newValue)
    ask >>= update <<< f
    pure unsubscribe
