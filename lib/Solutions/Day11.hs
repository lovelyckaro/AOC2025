module Solutions.Day11 where

import Control.Monad.Except
import Data.Containers.ListUtils
import Data.Map (Map, (!), (!?))
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

pEntry :: Parser (String, [String])
pEntry = (,) <$> some letterChar <* symbol ":" <*> some (lexeme (some letterChar))

pInp :: Parser (Map String [String])
pInp = M.fromList <$> some (lexemeLn pEntry)

pathsBetween :: Map String [String] -> Map (String, String) Int
pathsBetween graph = betweenMap
  where
    all = nubOrd . concat $ [(key : vals) | (key, vals) <- M.assocs graph]
    betweenMap = M.fromList [((from, to), between from to) | from <- all, to <- all]
    between from to
      | from == to = 1
      | otherwise = case graph !? from of
          Nothing -> 0
          Just nexts -> sum . catMaybes $ [betweenMap !? (next, to) | next <- nexts]

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  graph <- parseErr pInp inp
  return . T.show $ pathsBetween graph ! ("you", "out")

-- MATCH (svr)-(dac)-(fft)-(out) or (svr)-(fft)-(dac)-(out)
goodPathsOut :: Map String [String] -> Int
goodPathsOut graph = svr_dac_fft_out + svr_fft_dac_out
  where
    m = pathsBetween graph
    between src dest = m ! (src, dest)
    svr_dac_fft_out = between "svr" "dac" * between "dac" "fft" * between "fft" "out"
    svr_fft_dac_out = between "svr" "fft" * between "fft" "dac" * between "dac" "out"

part2 :: (MonadError String m) => PartSolution m
part2 = Solved $ \inp -> do
  graph <- parseErr pInp inp
  return . T.show . goodPathsOut $ graph

day11 :: (MonadError String m) => Solution m
day11 = Solution part1 part2
