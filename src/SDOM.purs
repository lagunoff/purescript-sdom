module SDOM
  ( SDOM
  , class ToNode
  , toNode
  , Gui
  , text
  , text_
  , Attr
  , unsafeAttr
  , handler
  , element
  , element_
  , ArrayChannel(..)
  , array
  , attach
  , unsafeSDOM
  , mapChannel
--  , withAsync
  , unSDOM
  ) where

import Data.TraversableWithIndex
import Prelude

import Control.Alternative (empty, (<|>))
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (length, modifyAt, unsafeIndex, updateAtIndices, (!!), (..))
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..), either)
import Data.Filterable (class Filterable, filterMap)
import Data.Filterable (filterMap, partitionMap)
import Data.Foldable (for_, oneOfMap, sequence_, traverse_)
import Data.List (List(..), drop, take, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import Debug.Trace (spy, trace, traceM)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, create, keepLatest, makeEvent, subscribe)
import Partial.Unsafe (unsafePartial)
import SDOM.GuiEvent (GuiEvent)
import SDOM.GuiEvent as GuiEvent
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node, NodeList, Text)
import Web.DOM.Document (createDocumentFragment, createElement, createTextNode, fromNode)
import Web.DOM.DocumentFragment as DocumentFragment
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, childNodes, lastChild, removeChild, replaceChild, setTextContent)
import Web.DOM.NodeList as NodeList
import Web.DOM.Text as Text
import Web.Event.Event as Event
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

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

type SDOM channel i o = Gui Element channel i o

class ToNode n where
  toNode :: n -> Node

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
unsafeSDOM
  :: forall ui channel i o
   . ( Track i
  -> Sink (GuiEvent ui channel i o)
  -> Effect { ui :: ui, unsubscribe :: Effect Unit })
  -> Gui ui channel i o
unsafeSDOM = Gui

-- | Change the event channel type of a component.
mapChannel
  :: forall ui channel channel' i o
   . (channel -> channel')
  -> Gui ui channel i o
  -> Gui ui channel' i o
mapChannel f (Gui create) =
  Gui \model sink -> create model (sink <<< GuiEvent.mapChannel f)

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

instance functorSDOM :: Functor (Gui ui channel i) where
  map f (Gui create) =
    Gui \model sink -> create model (sink <<< GuiEvent.mapFn (_ >>> f))

instance profunctorGui :: Profunctor (Gui ui channel) where
  dimap f g (Gui create) =
    Gui \model sink -> create (mapTrack f model) (sink <<< GuiEvent.mapFn (\fn -> g <<< fn <<< f))

instance strongSDOM :: Strong (Gui ui channel) where
  first (Gui create) =
    Gui \model sink -> create (mapTrack fst model) (sink <<< GuiEvent.mapFn (first))
  second (Gui create) =
    Gui \model sink -> create (mapTrack snd model) (sink <<< GuiEvent.mapFn (second))

instance lazyGui :: Lazy (Gui ui channel i o) where
  defer f = Gui \model -> unSDOM (f unit) model

unSDOM
  :: forall ui channel i o
   . Gui ui channel i o
  -> Track i
  -> Sink (GuiEvent ui channel i o)
  -> Effect { ui :: ui, unsubscribe :: Effect Unit }
unSDOM (Gui create) = create

-- | Create a component which renders a text node based on some part of the
-- | input model.
-- |
-- | The first argument is a function which chooses a `String` to render from
-- | the model. The function also has access to the context of the component.
-- |
-- | For example:
-- |
-- | ```
-- | > :type text \_ctx model -> model.title
-- | forall channel context a r.
-- |   SDOM channel context
-- |     { title :: String
-- |     | r
-- |     }
-- |     a
-- | ```
text :: forall channel i o. (i -> String) -> Gui Element channel i o
text f = Gui \{ ask, updates } _ -> do
  doc <- window >>= document
  model <- ask
  tn <- createTextNode (f model) (HTMLDocument.toDocument doc)
  unsubscribe <- updates `subscribe` \{ old, new } -> do
    let oldValue = f old
        newValue = f new
    when (oldValue /= newValue) $
      setTextContent newValue (Text.toNode tn)
  let ui = unsafeCoerce tn
  pure { ui, unsubscribe }

-- | Create a component which renders a (static) text node.
text_ :: forall channel i o. String -> Gui Element channel i o
text_ content = Gui \{ ask, updates } _ -> do
  doc <- window >>= document
  tn <- createTextNode content (HTMLDocument.toDocument doc)
  let ui = unsafeCoerce tn
      unsubscribe = pure unit
  pure { ui, unsubscribe }

-- | An attribute which can be associated with an `element`.
-- |
-- | The type arguments correspond to the context and model types of the resulting
-- | component.
-- |
-- | Attributes can be constructed using the functions in the `SDOM.Attributes`
-- | module, or unsafely using the `unsafeAttr` function.
-- |
-- | For example:
-- |
-- | ```
-- | > import SDOM.Attributes as A
-- | > :type A.type_ \_ model -> model.type
-- | forall context r.
-- |   Attr context
-- |     { "type" :: String
-- |     | r
-- |     }
-- | ```
data Attr model msg
  = Attr (Track model -> Element -> Effect (Effect Unit))
  | Handler (Effect model -> Sink msg -> Element -> Effect (Effect Unit))

-- | Create an attribute unsafely, by providing functions which initialize
-- | and update the attribute.
-- |
-- | _Note_: most applications should not require this function. Consider using
-- | the functions in the `SDOM.Attributes` module instead.
unsafeAttr
  :: forall model msg
   . (Track model -> Element -> Effect (Effect Unit))
  -> Attr model msg
unsafeAttr = Attr

-- | Create a `Handler` for specific events.
-- |
-- | The first argument is the name of the type of events to handle.
-- |
-- | The second argument is a function which produces a result from the raw DOM
-- | event object. The function also has access to the context of the component.
handler
  :: forall i e
   . String
  -> (i -> Event.Event -> e)
  -> Attr i e
handler evtName f = Handler \ask sink el -> do
  listener <- eventListener \ev -> ask >>= \model -> sink (f model ev)
  let target = Element.toEventTarget el
      unsubscribe = removeEventListener (wrap evtName) listener false target
  addEventListener (wrap evtName) listener false target
  pure unsubscribe

-- | Create a component which renders an element, including attributes, event
-- | handlers and a (static) list of child components.
-- |
-- | Instead of using this function directly, you probably will want to use the
-- | helper functions in the `SDOM.Elements` module.
-- |
-- | The first argument is the name of the element.
-- |
-- | The second argument is an array of attributes to attach to the rendered element.
-- |
-- | The third argument is an array of event handlers. Note that the result types
-- | of each handler is `Either channel (i -> o)`. That is, an event can _either_
-- | update the state of the current component (by providing a function of type
-- | `i -> o`), or it can use the _event channel_ to pass a message to a parent
-- | component.
-- |
-- | The fourth argument is a (static) array of child components.
-- |
-- | For example:
-- |
-- | ```
-- | > import SDOM.Elements as E
-- | > :type E.div [] [] [ text \_ _ -> "Hello, World!"]
-- | forall context channel i o.
-- |   SDOM context channel i o
-- |
-- | > import SDOM.Attributes as A
-- | > :type E.input [ A.value \_ model -> model.value ] [] []
-- | forall context channel o r.
-- |   SDOM context channel
-- |     { value :: String
-- |     | r
-- |     }
-- |     o
-- |
-- | > import SDOM.Events as Events
-- | > import Unsafe.Coerce (unsafeCoerce)
-- | > :paste
-- | > :type E.input
-- |     [ value \_ model -> model.value ]
-- |     [ change \_ e -> pure \model ->
-- |         model { value = (unsafeCoerce e).target.value }
-- |     ]
-- |     []
-- | ^D
-- | forall context channel o r.
-- |   SDOM context channel
-- |     { value :: String
-- |     | r
-- |     }
-- |     { value :: String
-- |     | r
-- |     }
-- | ```
element
  :: forall channel i o
   . String
  -> Array (Attr i (GuiEvent.GuiEvent Element channel i o))
  -> Array (Gui Element channel i o)
  -> Gui Element channel i o
element el attrs children = Gui \track@{ ask, updates } sink -> do
  doc <- window >>= document
  model <- ask
  e <- createElement el (HTMLDocument.toDocument doc)
  let setAttr :: Attr i (GuiEvent.GuiEvent Element channel i o) -> Effect (Effect Unit)
      setAttr (Attr setup) = setup track e
      setAttr (Handler setup) = setup ask sink e
  unsubscribers <- traverse setAttr attrs
  childrenEvts <- flip traverseWithIndex children \idx child -> do
    let childSink :: Sink (GuiEvent.GuiEvent Element channel i o)
        childSink (GuiEvent.EventEmit channel) = sink (GuiEvent.EventEmit channel)
        childSink (GuiEvent.EventStep fn) = sink (GuiEvent.EventStep fn)
        childSink (GuiEvent.EventRef el) = childNodes (Element.toNode e) >>= NodeList.item idx >>= case _ of
          Just oldEl -> void $ replaceChild (Element.toNode el) oldEl (Element.toNode e)
          Nothing -> pure unit
    { ui, unsubscribe } <- unSDOM child track childSink
    _ <- appendChild (Element.toNode ui) (Element.toNode e)
    pure unsubscribe
  pure
    { ui: e
    , unsubscribe:
        sequence_ unsubscribers
          *> traverse_ identity childrenEvts
    }

-- | Create a component which renders an element with a (static) array of child
-- | components, but no attributes or event handlers.
-- |
-- | Instead of using this function directly, you probably will want to use the
-- | helper functions in the `SDOM.Elements` module.
-- |
-- | For example:
-- |
-- | ```
-- | > import SDOM.Elements as E
-- | > :type E.div_ [ text \_ _ -> "Hello, World!"]
-- | forall context channel i o.
-- |   SDOM context channel i o
-- | ```
element_
  :: forall channel i o
   . String
  -> Array (Gui Element channel i o)
  -> Gui Element channel i o
element_ el = element el []

removeLastNChildren :: Int -> Node -> Effect Unit
removeLastNChildren m n = tailRecM loop m where
  loop toRemove
    | toRemove <= 0 = pure (Done unit)
    | otherwise = do
    child <- lastChild n
    case child of
      Nothing -> pure (Done unit)
      Just child_ -> do _ <- removeChild child_ n
                        pure (Loop (toRemove - 1))

-- | The event channel for an `array` component.
-- |
-- | An event is either passed to the next `Parent` in the chain, or handled
-- | `Here`, by acting on the array itself.
data ArrayChannel i channel
  = Parent channel
  | Here (Array i -> Array i)

-- | Create a component which renders an array of subcomponents.
-- |
-- | The first argument is the name of the HTML element used as the container.
-- |
-- | The second argument is a template component for rendered subcomponents.
-- |
-- | _Note:_
-- |
-- | - The context of the template component provides access to the index of
-- |   the current subcomponent.
-- | - The event channel for the template component provides the ability to
-- |   modify the input array itself.
-- | - This component is optimized for edits at the end of the array. Small
-- |   arrays should not present any issues, but large arrays might if edits
-- |   typically take place away from the end of the array.
array
  :: forall channel i
   . String
  -> Gui Element (ArrayChannel i channel) i i
  -> Gui Element channel (Array i) (Array i)
array el sd = Gui \{ ask, updates } sink -> do
  doc <- window >>= document
  models <- ask
  e <- createElement el (HTMLDocument.toDocument doc)
  unsubscribers <- Ref.new Nil
  let runUnsubscribers = Ref.read unsubscribers >>= sequence_
  let setup :: Array i -> Array i -> Effect Unit
      setup old_ new_
        | length new_ > length old_ = do
          for_ (length old_ .. (length new_ - 1)) \idx -> do
            let here xs = unsafePartial (xs `unsafeIndex` idx)
                track = { ask: ask >>= (here >>> pure), updates: chiildUpdates }
                chiildUpdates = filterMap (\{ old, new } -> { old: _, new: _ } <$> (old !! idx) <*> (new !! idx)) updates
                childSink (GuiEvent.EventEmit (Parent other)) = sink (GuiEvent.EventEmit other)
                childSink (GuiEvent.EventEmit (Here fi)) = sink (GuiEvent.EventStep fi)
                childSink (GuiEvent.EventRef ui) = childNodes (Element.toNode e) >>= NodeList.item idx >>= case _ of
                  Just oldEl -> void $ replaceChild (Element.toNode ui) oldEl (Element.toNode e)
                  Nothing -> pure unit
                childSink (GuiEvent.EventStep f) = sink (GuiEvent.EventStep \xs -> fromMaybe xs (modifyAt idx f xs))
            { ui, unsubscribe } <- unSDOM sd track childSink
            _ <- appendChild (Element.toNode ui) (Element.toNode e)
            _ <- Ref.modify (unsubscribe : _) unsubscribers
            pure unit
        | length new_ < length old_ = do
          let d = length old_ - length new_
          dropped <- Ref.modify' (\xs -> { state: drop d xs, value: take d xs }) unsubscribers
          sequence_ dropped
          removeLastNChildren d (Element.toNode e)
        | otherwise = pure unit
  setup [] models
  unsubscribe <- updates `subscribe` \{ old, new } -> setup old new
  pure
    { ui: e
    , unsubscribe: unsubscribe *> runUnsubscribers
    }

-- | Attach a component to the DOM.
-- |
-- | The first argument is the DOM `Element` which will contain the rendered
-- | component.
-- |
-- | The second argument is the initial model.
-- |
-- | The third argument is the component itself.
-- |
-- | The result contains two functions:
-- |
-- | - The `push` function allows the caller to provide additional model updates
-- |   which do not arise from user-generated events.
-- | - `The `detach` function detaches the component from the DOM and unregisters
-- |   any event handlers.
attach
  :: forall model
   . Element
  -> model
  -> Gui Element Void model model
  -> Effect
       { push :: (model -> model) -> Effect Unit
       , detach :: Effect Unit
       }
attach root model v = do
  modelRef <- Ref.new model
  document <- window >>= document
  { event, push } <- create
  updates <- create
  unsubscribe1 <- event `subscribe` \f -> do
    old <- Ref.read modelRef
    let new = f old
    traceM new
    Ref.write new modelRef
    updates.push { old, new }
  let track = { ask: Ref.read modelRef, updates: updates.event }
  let sink :: Sink (GuiEvent.GuiEvent Element Void model model)
      sink (GuiEvent.EventEmit channel) = absurd channel
      sink (GuiEvent.EventStep fn) = do
        traceM fn
        push fn
      sink (GuiEvent.EventRef el) = pure unit
  { ui, unsubscribe } <- unSDOM v track sink
  _ <- appendChild (Element.toNode ui) (Element.toNode root)
  pure
    { push: push
    , detach: unsubscribe *> unsubscribe1
    }
