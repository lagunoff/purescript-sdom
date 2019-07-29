module SDOM.GuiEvent
  ( GuiEvent(..)
  , mapChannel
  , mapGui
  , mapFn
  ) where

import Prelude

data GuiEvent ui channel i o
  = EventChan channel
  | EventGui ui
  | EventFn (i -> o)

mapChannel :: forall ui channel channel' i o. (channel -> channel') -> GuiEvent ui channel i o -> GuiEvent ui channel' i o
mapChannel f (EventChan channel) = EventChan $ f channel
mapChannel f (EventGui ui) = EventGui ui
mapChannel f (EventFn fn) = EventFn fn

mapGui :: forall ui ui' channel i o. (ui -> ui') -> GuiEvent ui channel i o -> GuiEvent ui' channel i o
mapGui f (EventGui ui) = EventGui $ f ui
mapGui f (EventChan channel) = EventChan channel
mapGui f (EventFn fn) = EventFn fn

mapFn :: forall ui channel i i' o o'. ((i -> o) -> (i' -> o')) -> GuiEvent ui channel i o -> GuiEvent ui channel i' o'
mapFn f (EventFn fn) = EventFn $ f fn
mapFn f (EventGui ui) = EventGui ui
mapFn f (EventChan channel) = EventChan channel
