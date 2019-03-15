module Config where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Maybe
import Data.Either
import Effect.Aff

import Node.Encoding (Encoding(..))

import Simple.JSON as JSON

import Helpers.Card (alert)
import Data.HTTP.Method
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J

--
import Affjax as AX


type Config =  {
  trelloAPIKey :: String,
  webServiceHost :: String,
  webServicePort :: String
}

-- TODO: Use the same function from the AJAX module
makeRequest :: forall a. (JSON.ReadForeign a) => String -> Aff (Either String a)
makeRequest url = do
  res <- AX.request ( AX.defaultRequest { url = url, method = Left GET, responseFormat = ResponseFormat.json } )
  case res.body of
    Left err -> pure $ Left $ AX.printResponseFormatError err
    Right json -> do
      case (JSON.readJSON (J.stringify json)) of
        Left err -> pure $ Left $ "An error occured while making a request to URL: " <> url
        Right (result) -> pure $ Right result

getConfig :: Aff (Either String Config)
getConfig = do
  let filePath = "config.json"
  url <- liftEffect $ getURL1 filePath
  res <- makeRequest url
  case res of
    Left err -> pure $ Left err
    Right (config :: Config) -> do
      -- liftEffect $ alert config.trelloAPIKey
      pure $ Right config
  --pure $ Right $ { trelloAPIKey: "", trelloToken: "" }

-- jsonStr <- readTextFile UTF8 ".config.json"
--   case JSON.readJSON jsonStr of
--     Left _ -> pure $ Left "Error while reading .config.json file"
--     Right (config :: Config) -> pure $ Right config
-- --

--  getConfig = do
--   jsonStr <- liftEffect $ readTextFile UTF8 ".config.json"
--   case JSON.readJSON jsonStr of
--     Left _ -> pure $ Left "Error while reading .config.json"
--     Right (config :: Config) -> pure $ Right config


foreign import data ChromeRuntime :: Type
foreign import getChromeRuntime :: Effect ChromeRuntime
foreign import getURL :: ChromeRuntime -> String -> Effect String

foreign import getURL1 :: String -> Effect String
foreign import getURL2 :: String -> Effect String
