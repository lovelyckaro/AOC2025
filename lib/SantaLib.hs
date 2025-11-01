{-# LANGUAGE OverloadedStrings #-}

module SantaLib
  ( fetchInput,
    fetchDescription,
    getInput,
    getExample,
    submitAnswer,
    readText,
    connected,
    toVector,
    getOpts,
    (|>),
    partSolution,
    Solution (..),
    PartSolution (..),
    module Advent,
  )
where

import Advent
import Advent.Types
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Foldable
import Data.Function ((&))
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector (Vector)
import Data.Vector qualified as V
import Log
import System.Environment (getEnv)
import Text.HTML.TagSoup
import Text.Pandoc

htmlToMd :: Text -> IO Text
htmlToMd html = runIOorExplode $ readHtml def html >>= writeMarkdown def

htmlToAnsi :: Text -> IO Text
htmlToAnsi html = runIOorExplode $ readHtml def html >>= writeANSI def

getOpts :: IO AoCOpts
getOpts = do
  year <- read <$> getEnv "AOC_YEAR"
  cookie <- getEnv "AOC_SESSION_COOKIE"
  repo <- T.pack <$> getEnv "AOC_AGENT_CODE_URL"
  email <- T.pack <$> getEnv "AOC_AGENT_EMAIL"
  return $ defaultAoCOpts (AoCUserAgent repo email) year cookie

fetchInput :: AoCOpts -> Day -> IO Text
fetchInput opts d = do
  inp <- runAoC_ opts $ AoCInput d
  TIO.writeFile ("input/day" <> show (dayInt d) <> ".input") inp
  return inp

fetchDescription :: AoCOpts -> Day -> IO ()
fetchDescription opts d = do
  m <- runAoC_ opts $ AoCPrompt d
  p1 <- htmlToMd $ fromMaybe "Part 1 not unlocked yet" (m !? Part1)
  p2 <- htmlToMd $ fromMaybe "Part 2 not unlocked yet" (m !? Part2)
  TIO.writeFile ("descr/day" <> show (dayInt d) <> "-part1.md") p1
  TIO.writeFile ("descr/day" <> show (dayInt d) <> "-part2.md") p2
  -- hopefully get parse the example. It is usually the first thing within <pre><code> tags.
  let example = pExample (m ! Part1)
  TIO.writeFile ("input/day" <> show (dayInt d) <> "-example.input") example

pExample :: Text -> Text
pExample html = fromTagText goal
  where
    tags = parseTags html
    beginsWithPre = partitions (isTagOpenName "pre") tags
    followedByCode = partitions (isTagOpenName "code") <$> beginsWithPre
    ((inners : _) : _) = followedByCode
    (goal : _) = filter isTagText inners

getExample :: Day -> IO Text
getExample n = TIO.readFile ("input/day" <> show (dayInt n) <> "-example.input")

getInput :: Day -> IO Text
getInput n = TIO.readFile ("input/day" <> show (dayInt n) <> ".input")

submitAnswer :: AoCOpts -> Day -> Part -> Text -> IO (Text, SubmitRes)
submitAnswer opts day part ans = do
  (response, submitRes) <- runAoC_ opts $ AoCSubmit day part $ T.unpack ans
  ansi <- htmlToAnsi response
  return (ansi, submitRes)

-- Common, useful algorithms:
readText :: (Read a) => Text -> a
readText = read . T.unpack

-- | Find connected subgraph given initial point and function handling neighbors
-- using breadth first search
connected :: forall f point. (Foldable f, Ord point) => (point -> f point) -> point -> [point]
connected neighbors initPoint = go neighbors (V.singleton initPoint) S.empty
  where
    go :: (point -> f point) -> Vector point -> Set point -> [point]
    go neighbors queue visited
      | null queue = toList visited
      | otherwise = go neighbors (rest <> unvisitedNeighbors) (S.insert current visited)
      where
        current = V.head queue
        rest = V.tail queue
        unvisitedNeighbors = current |> neighbors |> toVector |> V.filter (\p -> not (S.member p visited))

toVector :: (Foldable f) => f a -> Vector a
toVector = V.fromList . toList

(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixl 1 |>

-- | A solution for a part of a day
data PartSolution m = Unsolved | Solved (Text -> m Text)

-- | A solution for a day
data Solution m = Solution {part1, part2 :: PartSolution m}

partSolution :: (MonadIO m, MonadLog m, MonadError String m) => Solution m -> Part -> PartSolution m
partSolution solution Part1 = part1 solution
partSolution solution Part2 = part2 solution
