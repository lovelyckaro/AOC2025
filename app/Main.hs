{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (guard, when, (>=>))
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Log
import Log.Backend.StandardOutput
import Network.Wai
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Middleware.Cors
import Options.Applicative
import SantaLib
import SantaLib.Service
import SantaLib.Service (AocAPI)
import Servant
import Solutions
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

data InputSource = ExampleInput | AocUserInput | ExplicitInput Text
  deriving (Show, Eq)

inputSourceParser :: Parser InputSource
inputSourceParser =
  asum
    [ ExampleInput <$ flag' True (long "example" <> short 'x' <> help "Use example input"),
      ExplicitInput <$> option str (long "input" <> short 'i' <> help "Input for part" <> metavar "input"),
      AocUserInput <$ flag True True (long "userInput" <> help "Use user input (default)")
    ]

data RunConfiguration
  = Serve
      { port :: Int,
        logLevel :: LogLevel
      }
  | RunConfiguration
      { submit :: Bool,
        inputSource :: InputSource,
        day :: Day,
        part :: Part,
        logLevel :: LogLevel
      }
  deriving (Show)

cliConfigParser :: Parser RunConfiguration
cliConfigParser =
  RunConfiguration
    <$> switch (long "submit" <> short 's' <> help "Submit answer instead of just printing.")
    <*> inputSourceParser
    <*> (argument dayReader (help "Day to run" <> metavar "day") <|> option dayReader (long "day" <> short 'd' <> help "Day to run" <> metavar "day"))
    <*> (argument partReader (help "Part to run" <> metavar "part" <> value Part1) <|> option partReader (long "part" <> short 'p' <> help "Part to run" <> metavar "part"))
    <*> (option logLevelReader (long "loglevel" <> short 'l' <> help "Log level" <> metavar "loglevel" <> value defaultLogLevel))

serveConfigParser :: Parser RunConfiguration
serveConfigParser =
  (Serve <$ flag' True (long "serve" <> help "Run as restful service"))
    <*> option auto (long "port" <> short 'p' <> help "port" <> metavar "port" <> value 8080)
    <*> option logLevelReader (long "loglevel" <> short 'l' <> help "Log level" <> metavar "loglevel" <> value defaultLogLevel)

configParser :: Parser RunConfiguration
configParser = serveConfigParser <|> cliConfigParser

dayReader :: ReadM Day
dayReader = maybeReader $ readMaybe >=> mkDay

partReader :: ReadM Part
partReader = maybeReader $ \str -> do
  part <- readMaybe str
  case part of
    1 -> Just Part1
    2 -> Just Part2
    _ -> Nothing

logLevelReader :: ReadM LogLevel
logLevelReader = trace <|> info <|> attention
  where
    trace = maybeReader $ \str -> do
      let low = map toLower str
      guard (low == "trace")
      return LogTrace
    info = maybeReader $ \str -> do
      let low = map toLower str
      guard (low == "info")
      return LogInfo
    attention = maybeReader $ \str -> do
      let low = map toLower str
      guard (low == "attention")
      return LogAttention

configInfo :: ParserInfo RunConfiguration
configInfo = info (configParser <**> helper) fullDesc

unsolvedMessage :: Day -> Part -> Text
unsolvedMessage day part = "Day " <> T.show (dayInt day) <> ", part " <> T.show (partInt part) <> " is unsolved"

logError :: (MonadLog m) => ExceptT String m () -> m ()
logError except =
  runExceptT except >>= \case
    Right a -> return a
    Left err -> logAttention_ (T.pack err)

runCli :: (forall m x. LogT m x -> m x) -> Bool -> InputSource -> Day -> Part -> IO ()
runCli runLogger submit inputSource day part = runLogger $ logError $ do
  logTrace_ "Fetching description and input"
  aocOptions <- liftIO getOpts
  liftIO $ fetchDescription aocOptions day
  actualInput <- liftIO $ fetchInput aocOptions day
  exampleInput <- liftIO $ getExample day
  let input = case inputSource of
        ExampleInput -> exampleInput
        AocUserInput -> actualInput
        ExplicitInput i -> i
  let sol = solution day
  let partSol = partSolution sol part
  answer <- case partSol of
    Unsolved -> do
      logAttention_ (unsolvedMessage day part)
      liftIO exitFailure
    Solved answer -> do
      logTrace "Running solution " (day, part)
      answer input
  logInfo "Answer" answer
  if submit
    then do
      (response, result) <- liftIO $ submitAnswer aocOptions day part answer
      logInfo "Submit result" (T.pack (showSubmitRes result) <> "\n" <> response)
      case result of
        SubCorrect _ -> liftIO exitSuccess
        _ -> liftIO exitFailure
    else liftIO exitSuccess

corsAllowJson :: Middleware
corsAllowJson = cors (\_ -> Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"],
          corsMethods = "OPTIONS" : simpleMethods
        }

runServe :: Int -> (forall m x. LogT m x -> m x) -> IO ()
runServe port runLogging = do
  runLogging $ logInfo_ $ "AOC server listening to port " <> T.show port
  let loggingServer = hoistServer aocApi (runError . runLogging) (handleHealth :<|> handleAocRequest)
  let application = corsAllowJson $ serve aocApi loggingServer
  liftIO $ runEnv port application

type AppM = LogT (ExceptT String Handler)

runError :: ExceptT String Handler a -> Handler a
runError excep =
  runExceptT excep >>= \case
    Right a -> return a
    Left err -> throwError err400 {errBody = pack err} -- , errHeaders = ("Content-Type", "application/json") : err400.errHeaders }

handleHealth :: AppM Text
handleHealth = localDomain "health" $ do
  logTrace_ "health ok"
  return "ok"

throwServerError :: ServerError -> AppM a
throwServerError = lift . lift . throwError

handleAocRequest :: AocSolutionRequest -> AppM AocSolutionResponse
handleAocRequest req@AocSolutionRequest {..} = localDomain "handleAocRequest" $ do
  let sol = partSolution (solution day) part
  logTrace "Solution requested for " req
  case sol of
    Unsolved -> do
      let errMessage = "This day and part is still unsolved. Check again later."
      throwServerError err404 {errBody = errMessage}
    Solved f -> do
      ans <- f input
      return $ AocSolutionResponse req (Just ans)

main :: IO ()
main = do
  args <- execParser configInfo
  withStdOutLogger $ \logger ->
    case args of
      RunConfiguration {..} -> runCli (runLogT "aoc" logger logLevel) submit inputSource day part
      Serve {..} -> runServe port (runLogT "aoc" logger logLevel)
