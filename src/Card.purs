module Main where -- Card where


import Data.Maybe
import Prelude

import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (guard)
import Data.Array (head)
import Effect (Effect)
import Effect.Timer (setInterval, setTimeout)
import Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, alert, getElementById, showDialog, documentHead, setOnLoad,
                     jqry, dialog, JQuery, JQueryDialog, showModal, show) as Helpers
import Partial.Unsafe (unsafePartial)
import React as React
import React.DOM (text, a, div, span, img, form', fieldset', label', dialog, button', select', option') as DOM
import React.DOM.Props as Props
import ReactDOM as ReactDOM
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, getElementsByClassName, createElement, url) as DOM
import Web.DOM.Element (setAttribute, toNode, Element) as DOM
import Web.DOM.HTMLCollection (item, length)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (firstChild, insertBefore, appendChild) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, body) as DOM
import Web.HTML.Window (document) as DOM

main :: Effect Unit
main = do
 void $ setInterval 300 $ void $ do
   tryDisplay

tryDisplay :: Effect Boolean
tryDisplay = do
  window <- DOM.window
  document <- DOM.document window
  url <- DOM.url $ DOM.toDocument document
  result <- runMaybeT $ tryDisplayBtn url (DOM.toDocument document)
  case result of
    Nothing -> pure false
    Just _ -> pure true
  where
    doesElementExist :: Maybe DOM.Element -> Maybe Boolean
    doesElementExist Nothing  = Just true
    doesElementExist (Just _) = Nothing
    
    tryDisplayBtn :: String -> DOM.Document -> MaybeT Effect Unit
    tryDisplayBtn url document = do

      -- Do NOT display button if it's already been displayed
      _ <- MaybeT $ doesElementExist <$> (Helpers.getElementById "trello-reminders-btn" document)

      -- Do NOT display button if we are not in the context of a Card (we check this by verifying the URL)
      _ <- MaybeT $ pure $ Helpers.getCardIdFromUrl url
      
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
      secondChildOfOtherActions <- MaybeT $ Helpers.nextSibling (unsafeCoerce firstChildOfOtherActions)

      -- Add a new child before the first child of secondChildofotheractions
      pointOfInsertion <- MaybeT $ DOM.firstChild secondChildOfOtherActions
      _ <- lift $ DOM.insertBefore (unsafeCoerce trelloRemindersBtnDivElt) pointOfInsertion secondChildOfOtherActions
      _ <- lift $ ReactDOM.render (React.createLeafElement mainClass { }) trelloRemindersBtnDivElt
      pure unit


mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: { isJqueryLoaded: false, isJqueryUILoaded: false }, render: render <$> React.getState this, componentDidMount: didMount this }
    where
      didMount _this = do
        -- When our component is mounted, fetch & insert jQuery, jQueryUI Dialog
        -- ... and their corresponding Purescript modules
        window <- DOM.window
        document <- DOM.document window
        head <- Helpers.documentHead $ DOM.toDocument document
        
        jQueryScript <- DOM.createElement "script" $ DOM.toDocument document
        _ <- DOM.setAttribute "id" "jquery-script" jQueryScript
        --_ <- DOM.setAttribute "src" "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js" jQueryScript
        --_ <- DOM.setAttribute "async" "true" jQueryScript
        --_ <- Helpers.setOnLoad jQueryScript $ React.setState _this { isJqueryLoaded: true } -- Update the state once jQuery is loaded

        jQueryUIScript <- DOM.createElement "script" $ DOM.toDocument document
        _ <- DOM.setAttribute "id" "jquery-ui-script" jQueryUIScript
        --_ <- DOM.setAttribute "src" "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js" jQueryUIScript
        --_ <- DOM.setAttribute "async" "true" jQueryUIScript
        --_ <- Helpers.setOnLoad jQueryUIScript $ React.setState _this { isJqueryUILoaded: true } -- Update the state once jQueryUI is loaded
      
        _ <- DOM.appendChild (DOM.toNode jQueryScript) (DOM.toNode head)
        _ <- DOM.appendChild (DOM.toNode jQueryUIScript) (DOM.toNode head)

        pure unit
        
                
      render
        {
          isJqueryLoaded,
          isJqueryUILoaded
        } =
          DOM.div
          [
            Props.onClick onClick --,
            --if (isJqueryLoaded && isJqueryUILoaded) then Props.style {"pointerEvents": "auto"}
            --             else Props.style {"pointerEvents": "none"}
          ]
  
          [
            DOM.img
            [
              Props.src "chrome-extension://gjjpophepkbhejnglcmkdnncmaanojkf/images/iconspent.png",
              Props.className "agile-spent-icon-cardtimer"
            ],
            DOM.text "Set a reminder",
            DOM.dialog
            [
              Props._id "dialog-form",
              Props.hidden true,
              Props.title "Create new user"
            ]
            
            [
              DOM.form'
              [
                DOM.fieldset'
                [
                  DOM.label' [DOM.button' [DOM.text "Login with Trello"] ],
                  DOM.label'
                  [
                    DOM.text "Choose your plan",
                    DOM.select'
                    [
                      DOM.option'
                      [
                        DOM.text "Free (1 person, 5 reminders a week), Free"
                      ],
                      DOM.option'
                      [
                        DOM.text "Solo (1 person, unlimited reminders), $2.99 / month"
                      ],
                      DOM.option'
                      [
                        DOM.text "Team (5 people, unlimited reminders), $9.99 / month"
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]

      onClick evt = do
        window <- DOM.window
        document <- DOM.document window
        mDialog <- Helpers.getElementById "dialog-form" $ DOM.toDocument document
        case mDialog of
          Nothing -> pure unit
          Just dialog -> do
            Helpers.showModal dialog
            pure unit
