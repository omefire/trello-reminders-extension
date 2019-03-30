module Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, URL, alert, getElementById, documentHead, setOnLoad,
                     JQuery, JQueryDialog, showModal, show, setTimeout, setInterval, flatpickr, getElementsByClassName, showBootstrapModal,
                     hideBootstrapModal, formatDate) where

import Prelude

import Effect (Effect)

import Data.Nullable
import Data.Maybe(Maybe(..))

import Web.DOM.Internal.Types (Element, Node) as DOM
import Web.DOM.Document (getElementsByClassName, Document) as DOM
import Web.DOM.HTMLCollection (toArray)
import Data.Array (head)
import Web.HTML.Window (Window)

type URL = String

foreign import _getXFromUrl :: URL -> String -> Nullable String

getCardIdFromUrl :: URL -> Maybe String
getCardIdFromUrl url = toMaybe $ _getXFromUrl url "https://trello.com/c/"

getFirstElementByClassName :: String -> DOM.Document -> Effect (Maybe DOM.Element)
getFirstElementByClassName classNames doc = do
  eltsCollection <- DOM.getElementsByClassName classNames doc
  eltsArray <- toArray eltsCollection
  pure $ head eltsArray


getElementsByClassName :: String -> DOM.Document -> Effect (Array DOM.Element)
getElementsByClassName className doc = do
  eltsCollection <- DOM.getElementsByClassName className doc
  toArray eltsCollection

foreign import _nextSibling :: DOM.Node -> Effect (Nullable DOM.Node)

nextSibling :: DOM.Node -> Effect (Maybe DOM.Node)
nextSibling node = do
  sibling <- _nextSibling node
  pure $ toMaybe sibling 

foreign import alert :: forall a. a -> Effect Unit

foreign import _getElementById :: String -> DOM.Document -> Nullable DOM.Element

getElementById :: String -> DOM.Document -> Effect (Maybe DOM.Element)
getElementById id document = pure $ toMaybe $ _getElementById id document

-- foreign import _showDialog :: Effect Unit

-- showDialog :: Effect Unit
-- showDialog = _showDialog

foreign import _head :: DOM.Document -> DOM.Element
documentHead :: DOM.Document -> Effect DOM.Element
documentHead document = pure $ _head document

foreign import setOnLoad :: DOM.Element -> Effect Unit -> Effect Unit

foreign import data JQuery :: Type
foreign import data JQueryDialog :: Type
-- foreign import jqry :: String -> Effect JQuery
-- foreign import dialog :: forall r. JQuery -> Effect JQueryDialog

foreign import showModal :: DOM.Element -> Effect Unit
foreign import show :: DOM.Element -> Effect Unit

foreign import _setTimeout :: Effect Unit -> Int -> Effect Unit
setTimeout :: Int -> Effect Unit -> Effect Unit
setTimeout ms fn  = _setTimeout fn ms

foreign import _setInterval :: Effect Unit -> Int -> Effect Unit
setInterval :: Int -> Effect Unit -> Effect Unit
setInterval ms fn  = _setInterval fn ms

foreign import _flatpickr :: forall r. String -> { | r } -> Effect Unit
flatpickr :: forall r. String -> { | r } -> Effect Unit
flatpickr selector config = _flatpickr selector config


foreign import showBootstrapModal :: String -> Effect Unit
foreign import hideBootstrapModal :: String -> Effect Unit

foreign import formatDate :: String -> Effect String
