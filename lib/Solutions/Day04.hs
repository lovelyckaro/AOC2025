module Solutions.Day04 where

import Control.Monad
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)

type Graph = Set (Int, Int)

pInp :: Text -> Graph
pInp inp = rolls
  where
    xMax = T.length $ head $ T.lines inp
    yMax = length $ T.lines inp
    coords = [(x, y) | y <- [0 .. yMax - 1], x <- [0 .. xMax - 1]]
    chars = filter (/= '\n') $ T.unpack inp
    rolls = S.fromList . map fst . filter (\(_, c) -> c == '@') $ zip coords chars

neighbors :: Graph -> (Int, Int) -> [(Int, Int)]
neighbors s (x, y) = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  guard $ not (dx == 0 && dy == 0)
  let x' = x + dx
  let y' = y + dy
  guard $ (x', y') `S.member` s
  return (x', y')

part1 :: (Monad m) => PartSolution m
part1 = Solved $ \inp -> do
  let graph = pInp inp
  let rolls = S.toList graph
  let accessableRolls = filter (\neighbs -> length neighbs < 4) . map (neighbors graph) $ rolls
  return . T.show $ length accessableRolls

removeRolls :: Graph -> Graph
removeRolls g = S.filter (\p -> length (neighbors g p) >= 4) g

removeAllPossible :: Graph -> Graph
removeAllPossible g = let g' = removeRolls g in if g == g' then g else removeAllPossible g'

part2 :: (Monad m) => PartSolution m
part2 = Solved $ \inp -> do
  let graph = pInp inp
  let graph' = removeAllPossible graph
  let numRemoved = S.size graph - S.size graph'
  return $ T.show numRemoved

day04 :: (Monad m) => Solution m
day04 = Solution part1 part2
