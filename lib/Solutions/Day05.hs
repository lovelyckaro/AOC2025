module Solutions.Day05 where

import Control.Monad.Error.Class (MonadError)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pRange :: Parser (Int, Int)
pRange = (,) <$> decimal <* symbol "-" <*> decimal

pInp :: Parser ([(Int, Int)], [Int])
pInp = do
  ranges <- some $ lexemeLn pRange
  eol
  ingredients <- some $ lexemeLn decimal
  eof
  return (ranges, ingredients)

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges n = any (\(lo, hi) -> n >= lo && n <= hi) ranges

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  (ranges, ingredients) <- parseErr pInp inp
  let fresh = filter (isFresh ranges) ingredients
  return . T.show $ length fresh

{-

########
    #######

   ####
#####
-}

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (lo1, hi1) (lo2, hi2) = lo2 <= hi1 && lo2 >= lo1 || hi1 >= lo2 && hi1 <= hi2

merge :: (Int, Int) -> (Int, Int) -> (Int, Int)
merge (lo1, hi1) (lo2, hi2) = (min lo1 lo2, max hi1 hi2)

rangeSize :: (Int, Int) -> Int
rangeSize (lo, hi) = hi - lo + 1

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ranges = helper (sort ranges)
  where
    helper [] = []
    helper [range] = [range]
    helper (r1 : r2 : rest)
      | r1 `overlaps` r2 = helper (merge r1 r2 : rest)
      | otherwise = r1 : helper (r2 : rest)

countFresh :: [(Int, Int)] -> Int
countFresh = sum . map rangeSize . mergeRanges

part2 :: (MonadError String m) => PartSolution m
part2 = Solved $ \inp -> do
  (ranges, _) <- parseErr pInp inp
  let fresh = countFresh ranges
  return . T.show $ fresh

day05 :: (MonadError String m) => Solution m
day05 = Solution part1 part2
