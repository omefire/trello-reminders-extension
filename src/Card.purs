module Main where -- Card where


import Affjax.ResponseFormat
import Config
import Control.Monad.Except.Trans
import Data.Bifunctor
import Data.DateTime
import Data.Either
import Data.List.NonEmpty
import Data.List.Types
import Data.Maybe
import Data.Validation.Semigroup
import Effect.Aff
import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Control.MonadZero (guard)
-- import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Array (head, take, filter, (:), findIndex, updateAt, length, (!!))
import Data.Function (flip)
import Data.HTTP.Method (Method(..))
import Data.JSDate as JSDate
import Data.String.Common (trim, null) as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Timer (setInterval, setTimeout)
import Helpers.Card (getCardIdFromUrl, getFirstElementByClassName, nextSibling, alert, getElementById, documentHead, setOnLoad, showModal, show, setTimeout, setInterval, flatpickr, getElementsByClassName) as Helpers
import Partial.Unsafe (unsafePartial)
import React as React
import React.DOM (text, a, div, div', h5, span, i, span', img, form', form, fieldset', label', dialog, button', button, select', option', label, input, ul, li, p, table, tbody, tr', td', tr, br') as DOM
import React.DOM.Props as Props
import React.SyntheticEvent (SyntheticEvent_, SyntheticUIEvent', SyntheticEvent')
import ReactDOM as ReactDOM
import Simple.JSON as JSON
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document, getElementsByClassName, createElement, url) as DOM
import Web.DOM.Element (setAttribute, getAttribute, toNode, Element) as DOM
import Web.DOM.HTMLCollection (item)
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (firstChild, lastChild, insertBefore, appendChild, childNodes) as DOM
import Web.DOM.NodeList (toArray) as NL
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, body) as DOM
import Web.HTML.Window (document) as DOM
import AJAX (makeRequest) as AJAX

type UserID = { userID :: Int }

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


      -- Grab the Trello's idmember belonging to this user
      memberDivElt <- MaybeT $ Helpers.getFirstElementByClassName "member js-show-mem-menu" document
      trelloID <- MaybeT $ DOM.getAttribute "idmember" memberDivElt


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

      _ <- lift $ ReactDOM.render (React.createLeafElement setReminderClass { htmlDoc: document, trelloIDMember: trelloID }) trelloRemindersBtnDivElt
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
            $ (flip map) (take 10 frmErrs.errors) $ \err ->
                 case err.errorMessage of
                   "" -> DOM.text ""
                   _ -> DOM.li
                          [
                            Props.className "error"
                          ]
                          [
                            DOM.text $ err.errorMessage
                          ]

modalClass :: React.ReactClass { trelloIDMember :: String }
modalClass = React.component "Modal" component
  where
    component this =
      pure {
             state: {
                      name: "",
                      description: "",
                      emails: [] :: Array ({ emailID :: Int, emailValue :: String, isChecked :: Boolean }),
                      datetime: "",
                      formErrors: { errors: [] } :: FormErrors,

                      isNameValid: true,
                      isDescriptionValid: true,
                      isAtLeastOneEmailSelected: false,
                      isFormValid: false,
                      isLoadingEmails: false,
                      didErrorOccurWhileLoadingEmails: false,
                      errorThatOccuredWhileLoadingEmails: ""
                    },
             componentDidMount: componentDidMount this,
             render: render $ React.getState this
           }
      where
        componentDidMount that = do
          { trelloIDMember: trelloID } <- React.getProps that
          _ <- React.setState that { isLoadingEmails: true }
          runAff_ (\e ->
            case e of
              Left err -> do
                React.setState that { isLoadingEmails: false, didErrorOccurWhileLoadingEmails: true, errorThatOccuredWhileLoadingEmails: (show err) }

              Right emails -> React.setState that { emails: formatEmailsForUI emails, isLoadingEmails: false }
            )
            (do
                -- let a = 1
                -- pure $ [ { emailID: 1, emailValue: "omefire@gmail.com" } ]
              eEmails <-  (runExceptT $ do
                              user <- getTrelloData trelloID
                              uid <- getUserID user.email
                              -- _ <- liftEffect $ Helpers.alert $ (show uid)
                              emails <- getEmails $ uid.userID
                              pure $ emails) -- [{ emailID: 1, emailValue: "omefire@gmail.com" }])
              case eEmails of
                   Left err -> throwError $ error err
                   Right emails -> pure $ emails --[{ emailID: 1, emailValue: "omefire@gmail.com" }]
            )
            where
              formatEmailsForUI :: Array { emailID :: Int, emailValue :: String } -> Array { emailID :: Int, emailValue :: String, isChecked :: Boolean }
              formatEmailsForUI emails = (flip map) emails $ \e -> { emailID: e.emailID, isChecked: false, emailValue: e.emailValue }

              getUserID :: String -> ExceptT String Aff UserID
              getUserID email = do
                config@{ trelloAPIKey, trelloToken, webServiceHost, webServicePort } <- ExceptT getConfig
                let url = webServiceHost <> ":" <> webServicePort <> "/getUserIDForEmail/" <> email
                userID <- ExceptT $ AJAX.makeRequest url GET Nothing :: Aff (Either String UserID)
                pure userID

              getEmails :: Int -> ExceptT String Aff (Array { emailID :: Int, emailValue :: String })
              getEmails userid = do
                config@{ trelloAPIKey, trelloToken, webServiceHost, webServicePort } <- ExceptT getConfig
                let url = webServiceHost <> ":" <> webServicePort <> "/getEmailsForUser/" <> (show userid)
                emails <- ExceptT $ AJAX.makeRequest url GET Nothing :: Aff (Either String (Array { emailID :: Int, emailValue :: String })) -- { emailValue :: String, isChecked :: Boolean }
                pure emails

              getTrelloData :: String -> ExceptT String Aff TrelloUser
              getTrelloData trelloID = do
                config@{ trelloAPIKey, trelloToken } <- ExceptT getConfig
                let url = "https://api.trello.com/1/members/" <> trelloID <> "?key=" <> trelloAPIKey <> "&token=" <> trelloToken
                trelloUser <- ExceptT $ AJAX.makeRequest url GET Nothing :: Aff (Either String TrelloUser)
                pure trelloUser

        render state = do
          { name, formErrors, isNameValid, isDescriptionValid, isAtLeastOneEmailSelected, emails, isLoadingEmails, didErrorOccurWhileLoadingEmails, errorThatOccuredWhileLoadingEmails } <- state
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
                                    React.setState this { name: value }

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
                                        React.setState this { description: value }
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

                                DOM.div
                                [ Props.style { "overflow": "auto", "height": "200px" } ]
                                [
                                  DOM.table
                                  [
                                    Props.className "table table-striped table-dark"
                                  ]
                                  [
                                    DOM.tbody
                                    []
                                    $ displayEmails this isLoadingEmails emails didErrorOccurWhileLoadingEmails errorThatOccuredWhileLoadingEmails
                                    -- $ if (isLoadingEmails) then [ DOM.text "Loading emails, please wait..." ]
                                    --   else case (length emails) of
                                    --            0 -> [ DOM.text "Please, contact info@trelloreminders.com", DOM.br', DOM.text " to have email addresses added to your account." ]
                                    --            _ -> displayEmails this emails
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

                                   jsDate <- JSDate.parse dateString
                                   now <- JSDate.now
                                   { name, description, emails, datetime } <- React.getState this
                                   let emails' = map (\email -> email.emailValue) $ filter (\email -> email.isChecked == true) emails

                                   unV
                                    (\errors -> do

                                      let isNameValid = (length (filter (\error -> error.fieldName == "name") errors) == 0)
                                      let isDescriptionValid = (length (filter (\error -> error.fieldName == "description") errors) == 0)

                                      React.setState this { isNameValid: isNameValid, isDescriptionValid: isDescriptionValid, isFormValid: false, formErrors: { errors: errors }  }
                                    )
                                    (\formData -> do
                                      React.setState this { isNameValid: true, isDescriptionValid: true, formErrors: { errors: [] } }
                                      -- Submit data to server via AJAX
                                      runAff_
                                        (\e -> do
                                          case e of
                                            Left err -> Helpers.alert "failure"
                                            Right res -> Helpers.alert "success"
                                        )
                                        (do
                                          eConfig <- getConfig
                                          case eConfig of
                                            Left err -> throwError (error err)
                                            Right { webServiceHost, webServicePort } -> do
                                              let url = webServiceHost <> ":" <> webServicePort <> "/createReminder"
                                              name <- AJAX.makeRequest url POST (Just (J.fromString "{\"name\":\"Omar Mefire\", \"age\":\"12\"}")) :: Aff(Either String { name :: String }) -- J.fromObject
                                              pure name
                                        )
                                    )
                                    ( validate now $ { name: name, description: description, emails: emails', jsDate: jsDate } )

                                   pure unit

                              ]
                              [
                                DOM.text "Save changes"
                              ]
                          ]
                      ]
                  ]
              ]

setReminderClass :: React.ReactClass { htmlDoc :: DOM.Document, trelloIDMember :: String }
setReminderClass = React.component "Main" component
  where
  component this =
    pure {
            render: render this
         }
    where
      removeModalBackdrop doc = do
        mElt <- Helpers.getFirstElementByClassName "modal-backdrop" doc
        case mElt of
          Nothing -> pure unit
          Just elt -> DOM.setAttribute "class" "fade in" elt

      render that = do
        { htmlDoc: doc, trelloIDMember: idmember } <- React.getProps that
        pure $
          DOM.div'
          [
            DOM.span
            [
              Props.onClick $ \evt -> do
                 Helpers.setTimeout 500 $ do -- TODO: Couldn't this fail? Should we just keep doing it until it succeeds and then stop?
                   removeModalBackdrop doc
                 ,
              Props.unsafeMkProps "data-toggle" "modal",
              Props.unsafeMkProps "data-target" "#setreminderModal"
            ]
            [ DOM.text "Set a reminder" ],

            React.createLeafElement modalClass { trelloIDMember: idmember }
          ]



-- ====== Validation ======== --

type ValidatedFormData = { name :: String, description :: String, emails :: Array String, datetime :: DateTime }

type Error = { fieldName :: String, errorMessage :: String }
type Errors = Array Error

isNotEmpty :: String -> String -> V Errors String
isNotEmpty field value = let result = ((String.null <<< String.trim) value)
                         in case result of
                           true -> invalid $ [{ fieldName: field, errorMessage: "Please, provide a value for the  '" <> field <> "'" }]
                           false -> pure value


isDateValid :: String -> JSDate.JSDate -> V Errors DateTime
isDateValid field jsDate = case JSDate.toDateTime jsDate of
  Nothing -> invalid $ [{ fieldName: field, errorMessage: "Please, provide a valid datetime"}]
  Just date -> pure date


isDateInFuture :: String -> DateTime -> DateTime -> V Errors DateTime
isDateInFuture field currentDateTime dateTime
  | compare currentDateTime dateTime == LT = pure dateTime
  | otherwise = invalid $ [{ fieldName: field, errorMessage: "Please, make sure that the provided date is in the future"}]

isArrayNotEmpty :: forall a. String -> Array a -> V Errors (Array a)
isArrayNotEmpty field array
  | (length array == 0) = invalid $ [{ fieldName: field, errorMessage: "Please, check at least one " <> field <> " address" }]
  | otherwise = pure array


isEmailValid :: String -> Boolean
isEmailValid email = let re = Regex.regex """/\S+@\S+\.\S+/""" $ Regex.RegexFlags { ignoreCase: true, global: false, multiline: false, sticky: false, unicode: false }
                     in case re of
                       Left _ -> false
                       Right _ -> true

areEmailsValid :: Array String -> V Errors (Array String)
areEmailsValid emails = let ems = filter (\email -> isEmailValid email) emails
                        in case (length ems) of
                          0 -> invalid $ [{ fieldName: "emails", errorMessage: "There are invalid emails in the list" }]
                          _ -> pure emails


validate :: JSDate.JSDate -> { name :: String, description :: String, emails :: Array String, jsDate :: JSDate.JSDate } -> V Errors ValidatedFormData
validate now values =
  { name: _, description: _, emails: _, datetime: _ }
  <$> (isNotEmpty "name" values.name)
  <*> (isNotEmpty "description" values.description)
  <*> ( (isArrayNotEmpty "email" values.emails) `andThen` (\emails -> areEmailsValid emails) )
  <*> ( (isDateValid "datetime" values.jsDate)
        `andThen` (\dt -> isDateInFuture "dateinput" (unsafePartial fromJust $ JSDate.toDateTime now) dt)
      )


-- ======== JSON Deserialization ======== --
type TrelloUser =
 {
   id :: String,
   email :: String,
   username :: String,
   fullName :: String
 }


-- ======== Display ======== --
-- displayEmails :: ReactThis -> Boolean -> Array { emailValue :: String, isChecked :: Boolean } -> Boolean -> String -> Array ReactElement
displayEmails that isLoadingEmails emails
  didErrorOccurWhileLoadingEmails
  errorThatOccuredWhileLoadingEmails

  | isLoadingEmails = [ DOM.text "Loading emails. Please, wait..." ]
  | didErrorOccurWhileLoadingEmails = [ DOM.text $ "Sorry, an error occured while loading emails: " <> errorThatOccuredWhileLoadingEmails ]
  | (length emails == 0) = [ DOM.text "Please, contact info@trelloreminders.com", DOM.br',
                             DOM.text "to have emails registered to your account" ]
  | otherwise =  (flip map) (emails) $ \ email ->

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
                                                                         isChecked: (not e.isChecked),
                                                                         emailID: email.emailID
                                                                       } else e
                            )
              React.setState that { emails: emails' }
          ],
          DOM.text email.emailValue
        ]
      ]
    ]
