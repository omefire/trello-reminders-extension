module AJAX (makeRequest) where

import Prelude

import Simple.JSON as JSON
import Data.Argonaut.Core as J
import Affjax as AX
import Effect.Aff
import Data.Either
import Data.HTTP.Method
import Affjax.ResponseFormat as ResponseFormat
import Data.List.NonEmpty
import Foreign (ForeignError(..), MultipleErrors)

makeRequest :: forall a. (JSON.ReadForeign a) => String -> Aff (Either String a)
makeRequest url = do
  res <- AX.request ( AX.defaultRequest { url = url, method = Left GET, responseFormat = ResponseFormat.json } )
  case res.body of
    Left err -> do
      -- liftEffect $ Helpers.alert $ AX.printResponseFormatError err
      pure $ Left $ AX.printResponseFormatError err

    Right json -> do
      -- _ <- liftEffect $ Helpers.alert $ J.stringify json
      case (JSON.readJSON (J.stringify json)) of
        Left err -> do
          let errorStr = getErrorString err
          pure $ Left $ "An error occured while making a request to URL: " <> url <> ". " <> errorStr

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
