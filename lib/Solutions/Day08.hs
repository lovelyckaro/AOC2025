module Solutions.Day08 where

import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Error.Class (MonadError)
import Data.Graph qualified as G
import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

type Point = (Double, Double, Double)

pInp :: Parser [Point]
pInp = do
  let pPoint = (,,) <$> decimal <* symbol "," <*> decimal <* symbol "," <*> decimal
  ps <- many $ lexemeLn pPoint
  eof
  return ps

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt . sum . map (^ 2) $ [x1 - x2, y1 - y2, z1 - z2]

edges :: [Point] -> [((Point, Point), Double)]
edges points = do
  (start, possibleEnds) <- zip points (drop 1 $ tails points)
  end <- possibleEnds
  return $ ((start, end), distance start end)

smallestEdges :: Int -> [Point] -> [(Point, Point)]
smallestEdges n points = map fst . take n . sortOn snd $ edges points

mkGraph :: [(Point, Point)] -> G.Graph
mkGraph es = graph
  where
    adj = map (\(p, neighbs) -> (p, p, neighbs)) . M.toList . M.fromListWith (<>) $ [e | (p1, p2) <- es, e <- [(p1, [p2]), (p2, [p1])]]
    (graph, _, _) = G.graphFromEdges adj

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  points <- parseErr pInp inp
  let graph = mkGraph (smallestEdges 1000 points)
  let compSizes = map length $ G.components graph
  let largestThree = take 3 . reverse . sort $ compSizes
  return . T.show . product $ largestThree

lastConnectingEdge :: [Point] -> (Point, Point)
lastConnectingEdge points = binarySearch 1 Nothing
  where
    numPoints = length points
    es = map fst . sortOn snd . edges $ points
    binarySearch lo (Just hi) | lo > hi = es !! (lo - 1)
    binarySearch lo mbHi =
      let middle = case mbHi of
            Nothing -> lo * 2
            Just hi -> lo + (hi - lo) `div` 2
          graph = mkGraph (take middle es)
          numComponents = length $ G.components graph
          numVertices = length $ G.vertices graph
       in if numComponents > 1 || numVertices < numPoints
            then binarySearch (middle + 1) mbHi
            else binarySearch lo (Just (middle - 1))

part2 :: (MonadError String m) => PartSolution m
part2 = Solved $ \inp -> do
  points <- parseErr pInp inp
  let ((x1, _, _), (x2, _, _)) = lastConnectingEdge points
  return . T.show . round @_ @Integer $ x1 * x2

day08 :: (MonadError String m) => Solution m
day08 = Solution part1 part2
