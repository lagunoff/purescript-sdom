module Gui.SDOM
  ( SDOM
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
  , module Gui
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (length, modifyAt, unsafeIndex, (!!), (..))
import Data.Filterable (filterMap)
import Data.Foldable (for_, sequence_, traverse_)
import Data.List (List(..), drop, take, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (create, subscribe)
import Gui (Gui, GuiEvent(..), Sink, Track, mapRef, unGui, unsafeGui)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node)
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild, childNodes, lastChild, removeChild, replaceChild, setTextContent)
import Web.DOM.NodeList as NodeList
import Web.DOM.Text as Text
import Web.Event.Event as Event
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)

type SDOM channel i o = Gui Node channel i o


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
text :: forall channel i o. (i -> String) -> SDOM channel i o
text f = unsafeGui \{ ask, updates } _ -> do
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
text_ :: forall channel i o. String -> SDOM channel i o
text_ content = unsafeGui \{ ask, updates } _ -> do
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
  -> Array (Attr i (GuiEvent Element channel i o))
  -> Array (SDOM channel i o)
  -> SDOM channel i o
element el attrs children = unsafeGui \track@{ ask, updates } sink -> do
  doc <- window >>= document
  model <- ask
  e <- createElement el (HTMLDocument.toDocument doc)
  let setAttr :: Attr i (GuiEvent Element channel i o) -> Effect (Effect Unit)
      setAttr (Attr setup) = setup track e
      setAttr (Handler setup) = setup ask (sink <<< mapRef Element.toNode) e
  unsubscribers <- traverse setAttr attrs
  childrenEvts <- flip traverseWithIndex children \idx child -> do
    let childSink :: Sink (GuiEvent Node channel i o)
        childSink (EventEmit channel) = sink (EventEmit channel)
        childSink (EventStep fn) = sink (EventStep fn)
        childSink (EventRef el) = childNodes (Element.toNode e) >>= NodeList.item idx >>= case _ of
          Just oldEl -> void $ replaceChild el oldEl (Element.toNode e)
          Nothing -> pure unit
    { ui, unsubscribe } <- unGui child track childSink
    _ <- appendChild ui (Element.toNode e)
    pure unsubscribe
  pure
    { ui: Element.toNode e
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
  -> Array (SDOM channel i o)
  -> SDOM channel i o
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
  -> Gui Node (ArrayChannel i channel) i i
  -> Gui Node channel (Array i) (Array i)
array el sd = unsafeGui \{ ask, updates } sink -> do
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
                childSink (EventEmit (Parent other)) = sink (EventEmit other)
                childSink (EventEmit (Here fi)) = sink (EventStep fi)
                childSink (EventRef ui) = childNodes (Element.toNode e) >>= NodeList.item idx >>= case _ of
                  Just oldEl -> void $ replaceChild ui oldEl (Element.toNode e)
                  Nothing -> pure unit
                childSink (EventStep f) = sink (EventStep \xs -> fromMaybe xs (modifyAt idx f xs))
            { ui, unsubscribe } <- unGui sd track childSink
            _ <- appendChild ui (Element.toNode e)
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
    { ui: Element.toNode e
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
  -> SDOM Void model model
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
  let sink :: Sink (GuiEvent Node Void model model)
      sink (EventEmit channel) = absurd channel
      sink (EventStep fn) = do
        traceM fn
        push fn
      sink (EventRef el) = pure unit
  { ui, unsubscribe } <- unGui v track sink
  _ <- appendChild ui (Element.toNode root)
  pure
    { push: push
    , detach: unsubscribe *> unsubscribe1
    }
