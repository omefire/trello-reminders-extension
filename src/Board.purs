module Board where


import Prelude

import Effect (Effect)

import Data.Maybe (fromJust)

import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.DOM.Document (getElementsByClassName, createElement) as DOM
import Web.DOM.Element (setAttribute, toNode) as DOM
import Web.DOM.Node (firstChild, insertBefore) as DOM
import React.DOM (text) as DOM

import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import React as React
import ReactDOM as ReactDOM

import Web.DOM.HTMLCollection (item)

import Effect.Timer (setTimeout)

main :: Effect Unit
main = do 
  timeOutId <- setTimeout 2000 run
  pure unit


run :: Effect Unit
run = void $ do
  window <- DOM.window
  document <- DOM.document window
  collection <- DOM.getElementsByClassName "header-user" $  unsafeCoerce document
  element <- item 0 collection
  let element' = unsafePartial (fromJust element)

  newDivElement <- DOM.createElement "div" $ unsafeCoerce document
  DOM.setAttribute "id" "trello-reminders" newDivElement
  DOM.setAttribute "class" "header-btn" newDivElement
  mFirstChildOfHeaderUserNode <- DOM.firstChild (DOM.toNode element')
  let firstChildOfHeaderUserNode = unsafePartial (fromJust mFirstChildOfHeaderUserNode)
  _ <- DOM.insertBefore (DOM.toNode newDivElement) (firstChildOfHeaderUserNode) (DOM.toNode element')

  ReactDOM.render (React.createLeafElement mainClass { }) newDivElement

mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: { }, render: render }
    where
      render = do
        pure $ DOM.text "Trello Reminders"