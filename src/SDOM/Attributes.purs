module SDOM.Attributes
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

import SDOM (Attr, unsafeAttr)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (removeAttribute, setAttribute)
import Web.HTML.HTMLInputElement (setChecked, setDisabled, setValue)

attr :: forall model. String -> (model -> String) -> Attr model
attr attrName f =
  unsafeAttr \e ->
    { init: \model -> setAttribute attrName (f model) e
    , update: \{ old, new } -> do
        let oldValue = f old
            newValue = f new
        when (oldValue /= newValue) (setAttribute attrName newValue e)
    }

for :: forall model. (model -> String) -> Attr model
for = attr "for"

id :: forall model. (model -> String) -> Attr model
id = attr "id"

className :: forall model. (model -> String) -> Attr model
className = attr "class"

name :: forall model. (model -> String) -> Attr model
name = attr "name"

type_ :: forall model. (model -> String) -> Attr model
type_ = attr "type"

value :: forall model. (model -> String) -> Attr model
value f =
  unsafeAttr \e ->
    let update s = do
          setValue s (unsafeCoerce e)
          setAttribute "value" s e
     in { init: \model -> update (f model)
        , update: \{ old, new } -> do
            let oldValue = f old
                newValue = f new
            when (oldValue /= newValue) (update newValue)
        }

checked :: forall model. (model -> Boolean) -> Attr model
checked f =
  unsafeAttr \e ->
    let update b = do
          setChecked b (unsafeCoerce e)
          if b then setAttribute "checked" "checked" e
               else removeAttribute "checked" e
     in { init: \model -> update (f model)
        , update: \{ old, new } -> do
            let oldValue = f old
                newValue = f new
            when (oldValue /= newValue) (update newValue)
        }

disabled :: forall model. (model -> Boolean) -> Attr model
disabled f =
  unsafeAttr \e ->
    let update b = do
          setDisabled b (unsafeCoerce e)
          if b then setAttribute "disabled" "disabled" e
               else removeAttribute "disabled" e
     in { init: \model -> update (f model)
        , update: \{ old, new } -> do
            let oldValue = f old
                newValue = f new
            when (oldValue /= newValue) (update newValue)
        }
