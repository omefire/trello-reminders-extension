module Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, URL, alert, getElementById) where

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

foreign import _nextSibling :: DOM.Node -> Effect (Nullable DOM.Node)

nextSibling :: DOM.Node -> Effect (Maybe DOM.Node)
nextSibling node = do
  sibling <- _nextSibling node
  pure $ toMaybe sibling 

foreign import alert :: forall a. a -> Effect Unit

foreign import _getElementById :: String -> DOM.Document -> Nullable DOM.Element

getElementById :: String -> DOM.Document -> Effect (Maybe DOM.Element)
getElementById id document = pure $ toMaybe $ _getElementById id document
