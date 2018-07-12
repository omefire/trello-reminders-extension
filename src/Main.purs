module Main where


import Prelude

import Effect (Effect)

import Data.Maybe (fromJust)

import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.DOM.Document (getElementsByClassName) as DOM
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
  -- let node = DOM.toNonElementParentNode document
  collection <- DOM.getElementsByClassName "header-user" $  unsafeCoerce document
  element <- item 0 collection
  let element' = unsafePartial (fromJust element)
  ReactDOM.render (React.createLeafElement mainClass { }) element'

mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: { name: "Omar" }, render: pure $ DOM.text "Saltimbanque de potassium" }