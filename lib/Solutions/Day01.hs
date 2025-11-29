module Solutions.Day01 where

import Control.Arrow
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pInp :: Parser [(Int, Int)]
pInp = do
  nums <- many $ (,) <$> lexeme decimal <*> lexemeLn decimal
  eof
  return nums

part1 :: (MonadIO m, MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  nums <- parseErr pInp inp
  return $ T.show (length nums)

part2 :: PartSolution m
part2 = Unsolved

day01 :: (MonadIO m, MonadError String m) => Solution m
day01 = Solution part1 part2
