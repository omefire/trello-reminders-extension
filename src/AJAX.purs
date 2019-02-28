module AJAX (makeRequest) where

import Prelude

import Simple.JSON as JSON
import Data.Argonaut.Core as J
-- import Data.Argonaut.Core (Json)
import Affjax as AX
import Effect.Aff
import Data.Either
import Data.HTTP.Method
import Affjax.ResponseFormat as ResponseFormat
import Data.List.NonEmpty
import Foreign (ForeignError(..), MultipleErrors)
import Data.Maybe
import Affjax.RequestBody as ReqBody
import Effect.Class (liftEffect)
import Helpers.Card as Helpers
import Data.Bifunctor (lmap)

makeRequest :: forall a b. (JSON.ReadForeign a) => String -> Method -> Maybe J.Json -> Aff (Either String a)
makeRequest url method (Just content) = do
  res <- AX.request ( AX.defaultRequest { url = url, method = Left method, responseFormat = ResponseFormat.json, content = Just (ReqBody.Json content) } )
  case res.body of
    Left err -> do
      pure $ Left $ AX.printResponseFormatError err <> ". JSON data sent: " <> (J.stringify content)
    Right json_ -> do
      -- TODO: Test this code path.
      -- TODO: What if JSON.readJSON throws an exception? Would we correctly display it to the user for ease of debugging by the dev?
      case (JSON.readJSON $ J.stringify(json_)) of
        Left err -> do
          -- liftEffect $ Helpers.alert $ J.stringify(json_)
          pure $ Left $ "An error occured while parsing JSON after a request to: " <> url <> ". " <> (getErrorString err) <> ". Data returned from the server: " <> (J.stringify json_)
        Right (result) -> pure $ Right result

makeRequest url method Nothing = do
  res <- AX.request ( AX.defaultRequest { url = url, method = Left method, responseFormat = ResponseFormat.json, content = Nothing } )
  case res.body of
    Left err -> pure $ Left $ AX.printResponseFormatError err
    Right json -> do
      -- _ <- liftEffect $ Helpers.alert $ J.stringify json
      case (JSON.readJSON (J.stringify json)) of
        Left err -> pure $ Left $ "An error occured while parsing JSON after a request to: " <> url <> ". " <> (getErrorString err) <> ". Data returned from the server: " <> (J.stringify json)
        Right (result) -> pure $ Right result


getErrorString :: NonEmptyList ForeignError -> String
getErrorString errors = foldl (\str error ->
                                concatErrors error str
                              ) "" errors

concatErrors :: ForeignError -> String -> String
concatErrors (ForeignError s) str = s <> ", " <> str
concatErrors (TypeMismatch s1 s2) str = s1 <> " : " <> s2 <> ", " <> str
concatErrors (ErrorAtIndex i err) str = (show i) <> " : " <> (concatErrors err str)
concatErrors (ErrorAtProperty s err) str = s <> " : " <> (concatErrors err str)

