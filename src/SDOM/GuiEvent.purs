module SDOM.GuiEvent
  ( GuiEvent(..)
  , mapChannel
  , mapGui
  , mapFn
  ) where

import Prelude

data GuiEvent ui channel i o
  = EventEmit channel
  | EventRef ui
  | EventStep (i -> o)

mapChannel :: forall ui channel channel' i o. (channel -> channel') -> GuiEvent ui channel i o -> GuiEvent ui channel' i o
mapChannel f (EventEmit channel) = EventEmit $ f channel
mapChannel f (EventRef ui) = EventRef ui
mapChannel f (EventStep fn) = EventStep fn

mapGui :: forall ui ui' channel i o. (ui -> ui') -> GuiEvent ui channel i o -> GuiEvent ui' channel i o
mapGui f (EventRef ui) = EventRef $ f ui
mapGui f (EventEmit channel) = EventEmit channel
mapGui f (EventStep fn) = EventStep fn

mapFn :: forall ui channel i i' o o'. ((i -> o) -> (i' -> o')) -> GuiEvent ui channel i o -> GuiEvent ui channel i' o'
mapFn f (EventStep fn) = EventStep $ f fn
mapFn f (EventRef ui) = EventRef ui
mapFn f (EventEmit channel) = EventEmit channel
