module Main where -- Card where


import Data.Maybe
import Prelude

import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Effect (Effect)
import Effect.Timer (setInterval, setTimeout)
import Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, alert) as Helpers
import Partial.Unsafe (unsafePartial)
import React as React
import React.DOM (text) as DOM
import ReactDOM as ReactDOM
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, getElementsByClassName, createElement, url) as DOM
import Web.DOM.Element (setAttribute, toNode, Element) as DOM
import Web.DOM.HTMLCollection (item, length)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (firstChild, insertBefore) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument) as DOM
import Web.HTML.Window (document) as DOM

type CardDetails = {
  cardId :: String,
  sidebar :: DOM.Element,
  actionPane :: DOM.Element
}

main :: Effect Unit
main = do
 -- void $ setInterval 100 $ do
 --   isDisplayed <- display
 --   case isDisplayed of
 --     false -> void display
 --     true -> pure unit
 _ <- setInterval 200 $ do
   oldURL <- get
   newURL <- getURL
   if oldURL <> newURL then do
     display
     else 
 
 isDisplayed <- display
 case isDisplayed of
   false -> do
     -- void $ setInterval 100 main
     intervalId <- setInterval 100 main
     -- save intervalId in the state
     pure unit
   true -> do
     -- clear intervalId that is in the state
     pure unit

display :: Effect Boolean
display = do
  window <- DOM.window
  document <- DOM.document window
  url <- DOM.url $ DOM.toDocument document
  result <- runMaybeT $ compute url (DOM.toDocument document)
  case result of
    Nothing -> pure false
    Just _ -> pure true
  where
    compute :: String -> DOM.Document -> MaybeT Effect Unit
    compute url document = do
      cardId <- (MaybeT $ pure $ Helpers.getCardIdFromUrl url) :: MaybeT Effect String  
      windowSidebar <- MaybeT $ Helpers.getFirstElementByClassName "window-sidebar" document
      otherActions <- MaybeT $ Helpers.getFirstElementByClassName "other-actions" document
      trelloRemindersBtnDivElt <- lift $ DOM.createElement "a" document
      _ <- lift $ DOM.setAttribute "id" "trello-reminders-btn" trelloRemindersBtnDivElt
      _ <- lift $ DOM.setAttribute "class" "button-link" trelloRemindersBtnDivElt
      _ <- lift $ DOM.setAttribute "title" "Click to set a reminder on this card" trelloRemindersBtnDivElt

      -- Get the first child of other-actions/actionPane => <h3>Actions</h3>
      -- Get the next sibling of <h3>Actions</h3>
      -- In the next sibling of <h3>Actions</h3>:
      --   * Get the first child
      --   * Add a new child before the first child
      firstChildOfOtherActions <- MaybeT $ DOM.firstChild (unsafeCoerce otherActions)
      -- _ <- lift $ Helpers.alert $ firstChildOfOtherActions
      secondChildOfOtherActions <- MaybeT $ Helpers.nextSibling (unsafeCoerce firstChildOfOtherActions)
      -- _ <- lift $ Helpers.alert $ secondChildOfOtherActions

      -- Add a new child before the first child of secondChildofotheractions
      pointOfInsertion <- MaybeT $ DOM.firstChild secondChildOfOtherActions
      _ <- lift $ DOM.insertBefore (unsafeCoerce trelloRemindersBtnDivElt) pointOfInsertion secondChildOfOtherActions
      _ <- lift $ ReactDOM.render (React.createLeafElement mainClass { }) trelloRemindersBtnDivElt
      pure unit


mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: { }, render: render }
    where
      render = do
        pure $ DOM.text "Trello Reminders"
