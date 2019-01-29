module Main where -- Card where


import Data.DateTime
import Data.Maybe
import Data.Validation.Semiring
import Prelude

import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (guard)
import Data.Array (head, take, filter, (:), findIndex, updateAt, length, (!!))
import Data.Function (flip)
import Data.String.Common (trim, null) as String
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Timer (setInterval, setTimeout)
import Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, alert, getElementById, documentHead, setOnLoad, showModal, show, setTimeout, setInterval, flatpickr, getElementsByClassName) as Helpers
import Partial.Unsafe (unsafePartial)
import React as React
import React.DOM (text, a, div, div', h5, span, i, span', img, form', form, fieldset', label', dialog, button', button, select', option', label, input, ul, li, p, table, tbody, tr', td', tr) as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (SyntheticEvent_, SyntheticUIEvent', SyntheticEvent')
import ReactDOM as ReactDOM
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, getElementsByClassName, createElement, url) as DOM
import Web.DOM.Element (setAttribute, toNode, Element) as DOM
import Web.DOM.HTMLCollection (item)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (firstChild, lastChild, insertBefore, appendChild, childNodes) as DOM
import Web.DOM.NodeList (toArray) as NL
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

      -- Process:
      -- 1- Get the element with class 'window-sidebar'
      -- 2- Get the last child of 'window-sidebar': 'window-module u-clearfix'
      -- 3- Get the child element of 'window-module u-clearfix' with class 'u-clearfix': 'u-clearfix'
      -- 4- Within 'u-clearfix', create a new element: '<a class="button-link" ...'

      windowSidebar <- MaybeT $ Helpers.getFirstElementByClassName "window-sidebar" document
      lastChildOfWindowSidebar <- MaybeT $ DOM.lastChild (unsafeCoerce windowSidebar)
      childrenOfLastChildOfWindowSidebar_ <- lift $ DOM.childNodes lastChildOfWindowSidebar
      childrenOfLastChildOfWindowSidebar <- lift $ NL.toArray $ childrenOfLastChildOfWindowSidebar_
      uClearFix <- MaybeT $ pure $ childrenOfLastChildOfWindowSidebar !! 1
      firstChildOfUClearFix <- MaybeT $ DOM.firstChild uClearFix
      let pointOfInsertion = firstChildOfUClearFix

      trelloRemindersBtnDivElt <- lift $ DOM.createElement "a" document
      _ <- lift $ DOM.setAttribute "id" "trello-reminders-btn" trelloRemindersBtnDivElt
      _ <- lift $ DOM.setAttribute "class" "button-link" trelloRemindersBtnDivElt
      _ <- lift $ DOM.setAttribute "title" "Click to set a reminder on this card" trelloRemindersBtnDivElt


      _ <- lift $ DOM.insertBefore (unsafeCoerce trelloRemindersBtnDivElt) pointOfInsertion uClearFix

      _ <- lift $ ReactDOM.render (React.createLeafElement setReminderClass { htmlDoc: document }) trelloRemindersBtnDivElt
      pure unit


type FormErrors = { errors :: Array { fieldName :: String, errorMessage :: String } }

formErrorsClass :: React.ReactClass FormErrors
formErrorsClass = React.component "FormErrors" component
  where
    component this =
      pure {
             state: { },
             render: render $ React.getProps this
           }
      where
        render props = do
          frmErrs <- props
          pure $
            DOM.div
            [
              Props.className "formErrors"
            ]
            $ (flip map) (take 3 frmErrs.errors) $ \err ->
                 case err.errorMessage of
                   "" -> DOM.text ""
                   _ -> DOM.li
                          [
                            Props.className "error"
                          ]
                          [
                            DOM.text $ err.errorMessage
                          ]

modalClass :: React.ReactClass { }
modalClass = React.component "Modal" component
  where
    component this =
      pure {
             state: {
                      name: "",
                      description: "",
                      emails: [] :: Array ({ emailValue :: String, isChecked :: Boolean }),
                      datetime: "",
                      formErrors: { errors: [] } :: FormErrors,
                      isNameValid: true,
                      isDescriptionValid: true,
                      isAtLeastOneEmailSelected: false,
                      isFormValid: false
                    },
             componentDidMount: componentDidMount this,
             render: render $ React.getState this
           }
      where
        componentDidMount this = do
          -- TODO: Get from DB or Web service
          let emailsT = [ { emailValue: "omefire@gmail.com", isChecked: false }, { emailValue: "hamidmefire@gmail.com", isChecked: false } ]
          React.setState this { emails: emailsT }

        render state = do
          { name, formErrors, isNameValid, isDescriptionValid, isAtLeastOneEmailSelected, emails } <- state
          pure $
            DOM.div
              [
                Props.className "modal fade",
                Props._id "setreminderModal",
                Props.role "dialog",
                Props.unsafeMkProps "aria-labelledby" "setReminderCenterTitle",
                Props.unsafeMkProps "aria-hidden" "true"
              ]
              [
                DOM.div
                  [
                    Props.className "modal-dialog modal-dialog-centered",
                    Props.role "document"
                  ]
                  [
                    DOM.div
                      [ Props.className "modal-content" ]
                      [
                        DOM.div
                          [ Props.className "modal-header" ]
                          [
                            DOM.h5
                              [ Props.className "modal-title h2", Props._id "setReminderModalLongTitle" ]
                              [
                                DOM.text "Create a reminder"
                              ]
                          ],

                        DOM.div
                          [ Props.className "modal-body" ]
                          [
                            -- Display error messages
                            React.createLeafElement formErrorsClass formErrors,
                            DOM.form
                            [
                              Props.unsafeMkProps "novalidate" "",
                              Props.className "needs-validation was-validated"
                            ]
                            [
                              DOM.div
                              [ Props.className "form-group" ]
                              [
                                DOM.label
                                [
                                  Props.unsafeMkProps "for" "name-text-input",
                                  Props.className "col-form-label"
                                ]
                                [
                                  DOM.text "Name: "
                                ],

                                DOM.input
                                [
                                  Props._id "name-text-input",
                                  Props.className $ (if isNameValid then "form-control" else "form-control is-invalid"),
                                  Props._type "text",
                                  Props.placeholder "Enter a name for the reminder",
                                  Props.value name,

                                  Props.onChange $ \ evt -> do
                                    let value = (unsafeCoerce evt).target.value
                                    React.setStateWithCallback this { name: value } $ do
                                      let isNameEmpty = (String.null <<< String.trim) value
                                      case isNameEmpty of
                                         true -> do
                                                  let newErrors' = (filter (\e -> e.fieldName /= "name") formErrors.errors)
                                                  let newErrors = { fieldName: "name", errorMessage: "The 'name' field cannot be empty." } : newErrors'
                                                  React.setState this { isNameValid: false, formErrors: { errors: newErrors } }

                                         false -> do
                                                   let newErrors = (filter (\e -> e.fieldName /= "name") formErrors.errors)
                                                   React.setState this { isNameValid: true, formErrors: { errors: newErrors } }

                                  , Props.style { "width": "100%" }
                                ]
                              ],

                              DOM.div
                              [ Props.className "form-group" ]
                              [
                                DOM.label
                                [
                                  Props.unsafeMkProps "for" "description-text-input",
                                  Props.className "col-2 col-form-label"
                                ]
                                [
                                  DOM.text "Description: "
                                ],

                                DOM.div
                                [ Props.className "col-10" ]
                                [
                                  DOM.input
                                  [
                                    Props.className "form-control",
                                    Props.className $ (if isDescriptionValid then "form-control" else "form-control is-invalid"),
                                    Props._type "text",
                                    Props.placeholder "Enter a description for the reminder",
                                    Props._id "description-text-input",
                                    Props.style { "width": "100%" },

                                    Props.onChange $ \ evt -> do
                                        let value = (unsafeCoerce evt).target.value
                                        React.setStateWithCallback this { description: value } $ do
                                          let isDescriptionEmpty = (String.null <<< String.trim) value
                                          case isDescriptionEmpty of
                                               true -> do
                                                        let newErrors' = (filter (\e -> e.fieldName /= "description") formErrors.errors)
                                                        let newErrors = { fieldName: "description", errorMessage: "The 'description' field cannot be empty." } : newErrors'
                                                        React.setState this { isDescriptionValid: false, formErrors: { errors: newErrors } }

                                               false -> do
                                                         let newErrors = (filter (\e -> e.fieldName /= "description") formErrors.errors)
                                                         React.setState this { isDescriptionValid: true, formErrors: { errors: newErrors } }

                                  ]
                                ]
                              ],

                              DOM.div
                              [ Props.className "form-group" ]
                              [
                                DOM.label
                                [
                                  Props.unsafeMkProps "for" "emails-text-input",
                                  Props.className "col-form-label"
                                ]
                                [
                                  DOM.text "Emails: (Select the emails of people you want to remind)"
                                ],

                                DOM.div
                                [
                                  Props.className "alert alert-danger",
                                  Props.style { display: "none" } -- (if isAtLeastOneEmailSelected then "none" else "block")
                                ]
                                [
                                  DOM.text "Please, select at least one email address"
                                ],

                                DOM.table
                                [
                                  Props.className "table table-striped table-dark",
                                  Props.style { }
                                ]
                                [
                                  DOM.tbody
                                  []
                                  $ (flip map) (emails) $ \ email ->
                                        DOM.tr
                                        [ Props.style { "margin-left": "5px;" } ]
                                        [
                                          DOM.td'
                                          [
                                            DOM.label
                                            []
                                            [
                                              DOM.input
                                              [
                                                 Props._id email.emailValue,
                                                 Props.name email.emailValue,
                                                 Props._type "checkbox",
                                                 Props.value email.emailValue,
                                                 Props.checked email.isChecked,

                                                 Props.onInput $ \ evt -> do
                                                    let emails' = ((flip map) emails $ \ e ->
                                                         if e.emailValue == email.emailValue then { emailValue: email.emailValue,
                                                                                                    isChecked: (not e.isChecked) } else e
                                                    )
                                                    let isAtLeastOneEmailSelected' = ( length (filter (\em -> em.isChecked) emails') ) > 0
                                                    let newErrors' = (filter (\e -> e.fieldName /= "emails") formErrors.errors)

                                                    case isAtLeastOneEmailSelected' of
                                                       true -> do
                                                                React.setState this { emails: emails',
                                                                                      formErrors: { errors: newErrors' },
                                                                                      isAtLeastOneEmailSelected: true }
                                                       false -> do
                                                                 let newErrors = {
                                                                                   fieldName: "emails",
                                                                                   errorMessage: "Please, select at least one email address"
                                                                                 } : newErrors'
                                                                 React.setState this { emails: emails',
                                                                                       formErrors: { errors: newErrors },
                                                                                       isAtLeastOneEmailSelected: false }
                                              ],
                                              DOM.text email.emailValue
                                            ]
                                          ]
                                        ]
                                ]
                              ],


                              DOM.div
                              [ Props.className "form-group" ]
                              [
                                DOM.label
                                [
                                  Props.unsafeMkProps "for" "dateinput",
                                  Props.className "col-form-label"
                                ]
                                [
                                  DOM.text "Date & Time: (Specify when you want to be reminded)"
                                ],

                                DOM.div
                                [ Props.style { "width": "100%" } ]
                                [
                                  DOM.input
                                  [
                                    Props._id "dateinput",
                                    Props.className "form-control",
                                    Props._type "datetime-local",
                                    Props.placeholder "Pick a date & time",
                                    Props.style { "width": "100%" },
                                    Props.unsafeMkProps "data-format" "MM/dd/yyy hh:mm:ss",
                                    Props.required true
                                  ]
                                ]
                              ]
                            ]
                          ],

                        DOM.div
                          [ Props.className "modal-footer" ]
                          [
                            DOM.button
                              [ Props._type "button", Props.className "btn btn-secondary", Props.unsafeMkProps "data-dismiss" "modal" ]
                              [
                                DOM.text "Close"
                              ],

                            DOM.button
                              [
                                Props._type "button",
                                Props.className "btn btn-primary",
                                -- Props.unsafeMkProps "data-dismiss" "modal",
                                -- TODO: Validate every other field as well (when user clicks on submit button)
                                Props.onClick $ \ evt -> do
                                   window <- DOM.window
                                   document <- DOM.document window
                                   mElt <- Helpers.getElementById "dateinput" $ DOM.toDocument document
                                   let elt = (unsafePartial $ fromJust mElt) --TODO: What happens here if there is no element with the id we're looking for?
                                   let dateString = (unsafeCoerce elt).value
                                   let isEmpty = (\value -> (String.null <<< String.trim))
                                   let validate = \formData ->
                                      { name: _, description: _, emails: _, datetime: _ }
                                      <$> (isNotEmpty "name" formData.name)
                                      <*> (isNotEmpty "description" formData.description)
                                      <*> ( (isListNotEmpty formData.emails) `andThen` (\emails -> ) )
                                      <*> ( (isNotEmpty "datetime" formData.datetime) `andThen` (\datetime -> isDateValid datetime) `andThen` (\datetime -> isDateInFuture formData.datetime) )
                                   unV
                                    (\errors ->
                                      React.setState this { isNameValid:  }
                                    )
                                    (\formData -> do
                                      React.setState this { }
                                      pure unit
                                    )
                                    (validation $ FormData name description emails dateinput)

                                   -- React.setStateWithCallback this { name: value } $ do
                                   --   let isNameEmpty = (String.null <<< String.trim) value
                                   --   case isNameEmpty of
                                   --     true -> do
                                   --              let newErrors' = (filter (\e -> e.fieldName /= "date") formErrors.errors)
                                   --              let newErrors = { fieldName: "date", errorMessage: "Please, enter a valid date" } : newErrors'
                                   --              React.setState this { isNameValid: false, formErrors: { errors: newErrors } }

                                   --     false -> do
                                   --               let newErrors = (filter (\e -> e.fieldName /= "name") formErrors.errors)
                                   --               React.setState this { isNameValid: true, formErrors: { errors: newErrors } }

                              ]
                              [
                                DOM.text "Save changes"
                              ]
                          ]
                      ]
                  ]
              ]

setReminderClass :: React.ReactClass { htmlDoc :: DOM.Document }
setReminderClass = React.component "Main" component
  where
  component this =
    pure {
            render: render
         }
    where
      removeModalBackdrop doc = do
        mElt <- Helpers.getFirstElementByClassName "modal-backdrop" doc
        case mElt of
          Nothing -> pure unit
          Just elt -> DOM.setAttribute "class" "fade in" elt

      render = do
        pure $
          DOM.div'
          [
            DOM.span
            [
              Props.onClick $ \evt -> do
                 { htmlDoc: doc } <- React.getProps this
                 Helpers.setTimeout 500 $ do -- TODO: Couldn't this fail? Should we just keep doing it until it succeeds and then stop?
                   removeModalBackdrop doc
                 ,
              Props.unsafeMkProps "data-toggle" "modal",
              Props.unsafeMkProps "data-target" "#setreminderModal"
            ]
            [ DOM.text "Set a reminder" ],

            React.createLeafElement modalClass { }
          ]



-- ====== Validation ======== --
type Error = { fieldName :: String, errorMessage :: String }

isNotEmpty :: String -> String -> V Error String
isNotEmpty field value = let result = $ ((String.null <<< String.trim) value) == ""
                in case result of
                  true -> pure value
                  false -> invalid "The '" <> field <> "'" <> " cannot be empty"


isDateValid :: String -> V Error DateTime
isDateValid s = 


isDateInFuture :: DateTime -> V Error DateTime
isDateInFuture dt = 

isListNotEmpty :: forall a. [a] -> V Error [a]
isListNotEmpty list
  | (length list == 0) = pure


isEmailValid :: String -> Boolean
isEmailValid email =
