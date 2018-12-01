module Main where -- Card where


import Data.DateTime
import Data.Maybe
import Prelude

import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (guard)
import Data.Array (head, take, filter, (:), findIndex, updateAt, length)
import Data.Function (flip)
import Effect (Effect)
import Effect.Timer (setInterval, setTimeout)
import Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, alert, getElementById, showDialog, documentHead, setOnLoad, jqry, dialog, JQuery, JQueryDialog, showModal, show, setTimeout, setInterval, flatpickr) as Helpers
import Partial.Unsafe (unsafePartial)
import React as React
import React.DOM (text, a, div, div', h5, span, span', img, form', form, fieldset', label', dialog, button', button, select', option', label, input, ul, li, p, table, tbody, tr', td', tr) as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (SyntheticEvent_, SyntheticUIEvent', SyntheticEvent')
import ReactDOM as ReactDOM
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, getElementsByClassName, createElement, url) as DOM
import Web.DOM.Element (setAttribute, toNode, Element) as DOM
import Web.DOM.HTMLCollection (item)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (firstChild, insertBefore, appendChild) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, body) as DOM
import Web.HTML.Window (document) as DOM
import Data.String.Common (trim, null) as String

import Effect.Console (log, logShow)

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
                      isNameValid: false,
                      isDescriptionValid: false,
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
                                  Props.unsafeMkProps "for" "date-input",
                                  Props.className "col-form-label"
                                ]
                                [
                                  DOM.text "Date & Time: (Specify when you want to be reminded)"
                                ],

                                DOM.div
                                [ Props.className "input-group date", Props.style { "width": "100%" } ]
                                [
                                  DOM.input
                                  [
                                    Props.className "form-control", Props._type "text", Props.placeholder "Pick a date & time",
                                    Props._id "date-input", Props.style { "width": "100%" }
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
                              [ Props._type "button", Props.className "btn btn-primary", Props.unsafeMkProps "data-dismiss" "modal" ]
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

      initializeCalendar :: Effect Unit
      initializeCalendar = do
        Helpers.flatpickr "#date-text-input" { "enableTime": "true" }
        pure unit

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
                   initializeCalendar
                 ,
              Props.unsafeMkProps "data-toggle" "modal",
              Props.unsafeMkProps "data-target" "#setreminderModal"
            ]
            [ DOM.text "Set a reminder" ],

            React.createLeafElement modalClass { }
          ]

