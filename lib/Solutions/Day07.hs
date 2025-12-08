{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Solutions.Day07 where

import Control.Arrow (Arrow (second))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)

pInp :: Text -> (Set (Int, Int), (Int, Int), Int)
pInp inp = (rolls, start, yMax)
  where
    xMax = T.length $ head $ T.lines inp
    yMax = length $ T.lines inp
    coords = [(x, y) | y <- [0 .. yMax - 1], x <- [0 .. xMax - 1]]
    chars = filter (/= '\n') $ T.unpack inp
    rolls = S.fromList . map fst . filter (\(_, c) -> c == '^') $ zip coords chars
    start = head . map fst . filter (\(_, c) -> c == 'S') $ zip coords chars

next :: (Int, Int) -> (Int, Int)
next (x, y) = (x, y + 1)

split :: (Int, Int) -> [(Int, Int)]
split (x, y) = [(x - 1, y), (x + 1, y)]

step :: Set (Int, Int) -> Map (Int, Int) Int -> (Map (Int, Int) Int, Int)
step splitters beams = (splitted, M.size toSplit)
  where
    downOne = M.mapKeys next beams
    toSplit = M.restrictKeys downOne splitters
    unsplit = M.withoutKeys downOne splitters
    splitted = M.foldrWithKey (\p count m -> M.unionWith (+) m (M.fromListWith (+) [(p', count) | p' <- split p])) unsplit toSplit

numSplits :: Set (Int, Int) -> Int -> (Int, Int) -> Int
numSplits splitters yMax start = splits
  where
    (_, splits) = iterate (\(m, numSplits) -> second (+ numSplits) $ step splitters m) (M.singleton start 1, 0) !! yMax

part1 :: (Monad m) => PartSolution m
part1 = Solved $ \inp -> do
  let (splitters, start, yMax) = pInp inp
  let splits = numSplits splitters yMax start
  return . T.show $ splits

numQuantumSplits :: Set (Int, Int) -> Int -> (Int, Int) -> Int
numQuantumSplits splitters yMax start = sum finalBeams
  where
    finalBeams = iterate (fst . step splitters) (M.singleton start 1) !! yMax

part2 :: (Monad m) => PartSolution m
part2 = Solved $ \inp -> do
  let (splitters, start, yMax) = pInp inp
  let splits = numQuantumSplits splitters yMax start
  return . T.show $ splits

day07 :: (Monad m) => Solution m
day07 = Solution part1 part2
