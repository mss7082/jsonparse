module Lib where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:))
import Data.Aeson qualified as HTTP
import Data.Aeson qualified as JSON
import Data.Char (toLower)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import RIO

data User = User
  { userId :: !Int,
    userName :: !Text,
    userUsername :: !Text,
    userEmail :: !Text,
    userAddress :: !Address,
    userPhone :: !Text,
    userWebsite :: !Text,
    userCompany :: !Company
  }
  deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = JSON.genericParseJSON $ jsonOptions "user"

data Company = Company
  { companyName :: !Text,
    companyCatchPhrase :: !Text,
    companyBs :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Company where
  parseJSON = JSON.genericParseJSON $ jsonOptions "company"

data Geo = Geo
  { geoLat :: !Float,
    geoLng :: !Float
  }
  deriving (Eq, Show, Generic)

instance FromJSON Geo where
  parseJSON = JSON.withObject "Geo" $ \o -> do
    latString <- o .: "lat"
    lngString <- o .: "lng"
    return Geo {geoLat=read latString, geoLng=read lngString}

data Address = Address
  { addressStreet :: !Text,
    addressSuite :: !Text,
    addressCity :: !Text,
    addressZipcode :: !Text,
    addressGeo :: !Geo
  }
  deriving (Eq, Show, Generic)

instance FromJSON Address where
  parseJSON = JSON.genericParseJSON $ jsonOptions "address"

getUser :: IO (Either String [User])
getUser = do
  manager <- newTlsManager
  request <- HTTP.parseRequest "https://jsonplaceholder.typicode.com/users"
  (HTTP.responseBody >>> HTTP.eitherDecode) <$> HTTP.httpLbs request manager

jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
      lowercaseFirstCharacter [] = []
      lowercaseFirstCharacter (x : xs) = toLower x : xs
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter}

