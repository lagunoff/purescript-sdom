module SDOM.Events where

import SDOM
import Web.Event.Event as Event

change
  :: forall i e
  . (i -> Event.Event -> e)
  -> Attr i e
change = handler "change"

click
  :: forall i e
  . (i -> Event.Event -> e)
  -> Attr i e
click = handler "click"
