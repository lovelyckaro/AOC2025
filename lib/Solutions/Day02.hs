module Solutions.Day02 where

import Control.Monad.Error.Class
import Data.Set qualified as S
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pRange :: Parser (Integer, Integer)
pRange = do
  lo <- lexeme decimal
  symbol "-"
  hi <- lexeme decimal
  return $ (lo, hi)

pInp :: Parser [(Integer, Integer)]
pInp = do
  ranges <- pRange `sepBy` (symbol "," >> optional eol)
  optional eol >> eof
  return ranges

invalidInRange :: (Integer, Integer) -> [Integer]
invalidInRange (lo, hi) = filter invalid [lo .. hi]

invalid :: Integer -> Bool
invalid n = lo == hi
  where
    digits = ceiling $ logBase 10 (fromIntegral n)
    (lo, hi) = n `divMod` (10 ^ (digits `div` 2))

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  ranges <- parseErr pInp inp
  let answer = sum (ranges >>= invalidInRange)
  return $ T.show answer

groupsOf :: Int -> Integer -> S.Set T.Text
groupsOf size = S.fromList . T.chunksOf size . T.show

groupsOf' :: Int -> Integer -> S.Set Integer
groupsOf' size n =
  n
    |> iterate (`div` (10 ^ size))
    |> takeWhile (> 0)
    |> map (`mod` (10 ^ size))
    |> S.fromList

invalid' :: Integer -> Bool
invalid' n = any (\s -> S.size s == 1) groups
  where
    digits = ceiling $ logBase 10 (fromIntegral n)
    groups = [groupsOf size n | size <- [1 .. digits `div` 2]]

invalidInRange' :: (Integer, Integer) -> [Integer]
invalidInRange' (lo, hi) = filter invalid' [lo .. hi]

part2 :: (MonadError String m) => PartSolution m
part2 = Solved $ \inp -> do
  ranges <- parseErr pInp inp
  let answer = sum $ ranges >>= invalidInRange'
  return $ T.show answer

day02 :: (MonadError String m) => Solution m
day02 = Solution part1 part2
