module SDOM.Components where

import Prelude

import SDOM (Gui, text_)
import SDOM.Attributes as A
import SDOM.Elements as E
import SDOM.Events as Events
import SDOM.GuiEvent as GuiEvent
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

-- | Render a textbox component whose model is a `String`.
-- |
-- | _Note_: the model type can easily be changed using a lens.
textbox :: forall channel. Gui Element channel String String
textbox =
  E.input
    [ A.value \val -> val, Events.change \_ e -> GuiEvent.EventStep \_ -> (unsafeCoerce e).target.value ]
    []

-- | Render a checkbox and an accompanying `label` inside a `span`.
-- |
-- | The first argument chooses a unique name for the input component so that it
-- | can be connected to the label.
-- |
-- | The second and third arguments encapsulate the `checked` status of the
-- | checkbox as a getter/setter pair.
checkbox
  :: forall model channel
   . (model -> String)
  -> (model -> Boolean)
  -> (model -> Boolean -> model)
  -> Gui Element channel model model
checkbox name getChecked setChecked =
  E.span_
    [ E.input
        [ A.type_ \_ -> "checkbox"
        , A.checked \model -> getChecked model
        , A.id \model -> name model
        , Events.change \_ e -> GuiEvent.EventStep \model ->
            setChecked model $ not $ getChecked model
        ]
        []
    , E.label
        [ A.for \model -> name model ]
        []
    ]

-- | Render a select component.
-- |
-- | The first and second arguments encapsulate the selected option
-- | as a getter/setter pair on the model type.
-- |
-- | The third argument converts an option to a unique key and a rendered label.
-- |
-- | The fourth argument converts a key back into an option.
-- |
-- | The fifth argument is an array of all available options.
select
  :: forall option channel
   . (option -> { key :: String, label :: String })
  -> (String -> option)
  -> Array option
  -> Gui Element channel option option
select fromOption toOption options =
  E.select
    [ A.value \value -> (fromOption value).key
    , Events.change \_ e -> GuiEvent.EventStep \_ -> toOption (unsafeCoerce e).target.value
    ]
    (options <#> \option ->
      let { key, label } = fromOption option
       in E.option [ A.value \_ -> key ] [ text_ label ]
    )
