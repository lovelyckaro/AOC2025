module Solutions.Day03 where

import Data.List (tails)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)

pInp :: Text -> [[Int]]
pInp txt = [[readText digit | digit <- T.chunksOf 1 line] | line <- T.lines txt]

maxJoltage :: Int -> [Int] -> Int
maxJoltage digits (firstNum : battery) = helper digits (firstNum, battery) battery
  where
    helper 0 _ _ = 0
    helper 1 (currMax, _) [] = currMax
    helper n (currMax, (next : currMaxtail)) [] = currMax * 10 ^ (n - 1) + helper (n - 1) (next, currMaxtail) currMaxtail
    helper n (currMax, currMaxtail) (nextCandidate : unchecked)
      | nextCandidate > currMax && length unchecked >= (n - 1) = helper n (nextCandidate, unchecked) unchecked
      | otherwise = helper n (currMax, currMaxtail) unchecked

part1 :: (Monad m) => PartSolution m
part1 = Solved $ \inp -> do
  let batteries = pInp inp
  let answer = sum $ map (maxJoltage 2) batteries
  return $ T.show answer

part2 :: (Monad m) => PartSolution m
part2 = Solved $ \inp -> do
  let batteries = pInp inp
  let answer = sum $ map (maxJoltage 12) batteries
  return $ T.show answer

day03 :: (Monad m) => Solution m
day03 = Solution part1 part2
