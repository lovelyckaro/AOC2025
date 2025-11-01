{-# LANGUAGE OverloadedStrings #-}

module SantaLib.Service where

import Advent.Types
import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Servant.API
import Text.Read (readEither, readMaybe)

parseDay :: Text -> Either Text Day
parseDay txt = do
  num <- maybe (Left $ "invalid day: " <> txt) Right $ readMaybe (T.unpack txt)
  maybe (Left $ "invalid day: " <> T.show num) Right $ mkDay num

parsePart :: Text -> Either Text Part
parsePart txt = do
  num <- maybe (Left $ "invalid part: " <> txt) Right $ readMaybe (T.unpack txt)
  case num of
    1 -> return Part1
    2 -> return Part2
    _ -> Left $ "invalid part: " <> T.show num

instance FromHttpApiData Day where
  parseUrlPiece = parseDay
  parseHeader = parseHeader >=> parseDay
  parseQueryParam = parseDay

instance FromHttpApiData Part where
  parseUrlPiece = parsePart
  parseHeader = parseHeader >=> parsePart
  parseQueryParam = parsePart

data AocSolutionRequest = AocSolutionRequest
  { day :: Day,
    part :: Part,
    input :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AocSolutionRequest where
  toJSON AocSolutionRequest {..} = object ["day" .= dayInt day, "part" .= partInt part, "input" .= input]

instance FromJSON AocSolutionRequest where
  parseJSON = withObject "AocSolutionRequest" $ \request ->
    AocSolutionRequest
      <$> parseDay request
      <*> parsePart request
      <*> (request .: "input")
    where
      parseDay req = do
        day <- req .: "day"
        case mkDay day of
          Nothing -> fail $ "parsing day failed. Expected number between 1 and 25. actual: " <> show day
          Just d -> return d
      parsePart req = do
        part :: Int <- req .: "part"
        case part of
          1 -> return Part1
          2 -> return Part2
          _ -> fail $ "parsing part failed. Expected number 1 or 2. actual: " <> show part

data AocSolutionResponse = AocSolutionResponse
  { req :: AocSolutionRequest,
    answer :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON AocSolutionResponse

instance FromJSON AocSolutionResponse

type HealthAPI = "health" :> Get '[JSON] Text

healthApi :: Proxy HealthAPI
healthApi = Proxy @HealthAPI

type AocAPI =
  HealthAPI
    :<|> ReqBody '[JSON] AocSolutionRequest
      :> Get '[JSON] AocSolutionResponse

aocApi :: Proxy AocAPI
aocApi = Proxy @AocAPI
