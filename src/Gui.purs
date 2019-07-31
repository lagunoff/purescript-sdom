module Gui
  ( Sink
  , Track
  , mapTrack
  , Gui
  , GuiEvent(..)
  , mapEmit
  , mapRef
  , mapStep
  , mapChannel
  , unsafeGui
  , unGui
  ) where


import Prelude

import Control.Lazy (class Lazy)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Tuple (fst, snd)
import Effect (Effect)
import FRP.Event (Event)

-- | A value of type `SDOM channel context i o` represents a component in the
-- | "static DOM".
-- |
-- | Simple components can be created using the `text` and `element` functions.
-- | The `array` function can be used to create a component which renders a
-- | uniform array of subcomponents. The `SDOM.Components` module also contains
-- | some ready-to-use components.
-- |
-- | Here is an explanation of each type variable:
-- |
-- | - `i` is the type of the model (when used as an input).
-- |   Inputs of type `i` will be provided in order to initialize or rerender
-- |   the component.
-- | - `o` is the type of the model (when used as an output).
-- |   Events raised by the component may change the model by providing a function
-- |   of type `i -> o`. The model is split into input and output type arguments
-- |   to allow the profunctor instances for `SDOM` to exist, and to enable the
-- |   use of profunctor lenses for component composition.
-- | - `context` is the type of the "context" of the component. If the component
-- |   is rendered as a child of a dynamically-sized list, the context will include
-- |   its index in that list, for example. This type argument may not be needed
-- |   in simple components.
-- | - `channel` is the type of the "event channel" of the component. If the
-- |   component is rendered as a child of a dynamically-sized list, the channel
-- |   type will provide a way to pass an event to the owner of that list, so that
-- |   the component can modify the list itself, not just the element of the list
-- |   which generated it. For example, we might use the channel to allow a
-- |   component to remove itself from a list.
-- |
-- | Since `SDOM` is a _strong profunctor_, we can apply profunctor lenses to values
-- | of type `SDOM channel context i o` directly, to focus a component on a
-- | particular piece of the model:
-- |
-- | ```
-- | > :type text (const identity)
-- | forall channel context a. SDOM channel context String a
-- |
-- | > import Data.Lens
-- | > :type _1 (text (const identity))
-- | forall channel context a b.
-- |   SDOM channel context
-- |     (Tuple String b)
-- |     (Tuple a b)
-- | ```
newtype Gui ui channel i o = Gui
   ( Track i
  -> Sink (GuiEvent ui channel i o)
  -> Effect { ui :: ui, unsubscribe :: Effect Unit })

type Sink msg = msg -> Effect Unit

type Track a = { ask :: Effect a, updates :: Event { old :: a, new :: a } }

mapTrack :: forall a b. (a -> b) -> Track a -> Track b
mapTrack f o = { ask, updates } where
  ask = map f o.ask
  updates = map (\{ old, new } -> { old: f old, new: f new }) o.updates

data GuiEvent ui channel i o
  = EventEmit channel
  | EventRef ui
  | EventStep (i -> o)

mapEmit
  :: forall ui channel channel' i o
   . (channel -> channel')
  -> GuiEvent ui channel i o
  -> GuiEvent ui channel' i o
mapEmit f (EventEmit channel) = EventEmit $ f channel
mapEmit f (EventRef ui) = EventRef ui
mapEmit f (EventStep fn) = EventStep fn

mapRef
  :: forall ui ui' channel i o
   . (ui -> ui')
  -> GuiEvent ui channel i o
  -> GuiEvent ui' channel i o
mapRef f (EventRef ui) = EventRef $ f ui
mapRef f (EventEmit channel) = EventEmit channel
mapRef f (EventStep fn) = EventStep fn

mapStep
  :: forall ui channel i i' o o'
   . ((i -> o) -> (i' -> o'))
  -> GuiEvent ui channel i o
  -> GuiEvent ui channel i' o'
mapStep f (EventStep fn) = EventStep $ f fn
mapStep f (EventRef ui) = EventRef ui
mapStep f (EventEmit channel) = EventEmit channel


-- | Change the event channel type of a component.
mapChannel
  :: forall ui channel channel' i o
   . (channel -> channel')
  -> Gui ui channel i o
  -> Gui ui channel' i o
mapChannel f (Gui create) =
  Gui \model sink -> create model (sink <<< mapEmit f)

-- | A convenience function which provides the ability to use `Event`s
-- | directly in a component's event channel.
-- |
-- | `Event`s will be disposed of when the component unmounts, or when a new
-- | event takes its place.
-- |
-- | For example, clicking this button starts a timer which raises a `Unit`
-- | event every second.
-- |
-- | ```
-- | > :type text (const identity)
-- | forall channel context a. SDOM channel context String a
-- |
-- | > import SDOM.Elements as E
-- | > import SDOM.Events as Events
-- |
-- | > handler _ _ = Left (interval 1000 $> Left unit)
-- |
-- | > :type withAsync (E.button [] [Events.click handler] [ text \_ _ -> "Start" ])
-- | forall channel context model. SDOM Unit channel context model
-- | ```
-- withAsync
--   :: forall ui channel i o
--    . Gui ui (Event (GuiEvent ui channel i o)) i o
--   -> Gui ui channel i o
-- withAsync = ?withAsync keepLatest

instance functorGui :: Functor (Gui ui channel i) where
  map f (Gui create) =
    Gui \model sink -> create model (sink <<< mapStep (_ >>> f))

instance profunctorGui :: Profunctor (Gui ui channel) where
  dimap f g (Gui create) =
    Gui \model sink -> create (mapTrack f model) (sink <<< mapStep (\fn -> g <<< fn <<< f))

instance strongGui :: Strong (Gui ui channel) where
  first (Gui create) =
    Gui \model sink -> create (mapTrack fst model) (sink <<< mapStep (first))
  second (Gui create) =
    Gui \model sink -> create (mapTrack snd model) (sink <<< mapStep (second))

instance lazyGui :: Lazy (Gui ui channel i o) where
  defer f = Gui \model -> unGui (f unit) model


-- | This function is provided in order to wrap existing Javascript components.
-- |
-- | Most applications should not need to use this function directly. Instead,
-- | you can build components using the other, safe functions exposed by this
-- | module, or reuse components from the `SDOM.Components` module.
-- |
-- | This function accepts a function as its only argument. This function should:
-- |
-- | - Set up any DOM components and render the initial model,
-- | - Subscribe to model updates in order to update those components,
-- | - Return an `events` stream for user events generated by the component,
-- | - Return an `unsubscribe` function to clean up any event handlers when the
-- |   component is removed.
unsafeGui
  :: forall ui channel i o
   . ( Track i
  -> Sink (GuiEvent ui channel i o)
  -> Effect { ui :: ui, unsubscribe :: Effect Unit })
  -> Gui ui channel i o
unsafeGui = Gui

unGui
  :: forall ui channel i o
   . Gui ui channel i o
  -> Track i
  -> Sink (GuiEvent ui channel i o)
  -> Effect { ui :: ui, unsubscribe :: Effect Unit }
unGui (Gui create) = create
