module Solutions.Day01 where

import Control.Monad.Error.Class
import Data.List (scanl')
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (count)

pDir :: Parser Int
pDir = do
  dir <- 1 <$ string "R" <|> (-1) <$ string "L"
  amount <- decimal
  return $ dir * amount

pInp :: Parser [Int]
pInp = do
  rotations <- some (lexemeLn pDir)
  eof
  return rotations

sums :: [Int] -> [Int]
sums = scanl' plusMod100 50
  where
    plusMod100 x y = (x + y) `mod` 100

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  nums <- parseErr pInp inp
  return . T.show . count (== 0) . sums $ nums

explode :: Int -> [Int]
explode n = replicate (abs n) (signum n)

part2 :: (MonadError String m) => PartSolution m
part2 = Solved $ \inp -> do
  nums <- parseErr pInp inp
  let exploded = nums >>= explode
  return . T.show . count (== 0) . sums $ exploded

day01 :: (MonadError String m) => Solution m
day01 = Solution part1 part2
