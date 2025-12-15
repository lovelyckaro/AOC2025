module Solutions.Day09 where

import Control.Monad.Error.Class (MonadError)
import Data.Bifunctor (bimap)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List
import Data.Map (Map, (!))
import Data.Map qualified as M
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

type Point = (Int, Int)

type CPoint = (Int, Int) -- compressed point

pInp :: Parser [Point]
pInp = do
  let pPoint = (,) <$> lexeme decimal <* symbol "," <*> lexeme decimal
  points <- many $ lexemeLn pPoint
  eof
  return points

combos :: [Point] -> [(Point, Point)]
combos points = do
  (p1, p2s) <- zip points . drop 1 . tails $ points
  p2 <- p2s
  return (p1, p2)

area :: (Point, Point) -> Int
area ((x1, y1), (x2, y2)) = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  points <- parseErr pInp inp
  let largest = maximumBy (comparing area) $ combos points
  return . T.show . area $ largest

perimeter :: [Point] -> [(Point, Point)]
perimeter points = zip points (drop 1 points <> take 1 points)

compress :: [Point] -> ([CPoint], Map CPoint Point)
compress points = (compressed, decompressMap)
  where
    ordDistinct = IS.toAscList . IS.fromList
    xMap = IM.fromAscList $ zip (ordDistinct . map fst $ points) [0 ..]
    yMap = IM.fromAscList $ zip (ordDistinct . map snd $ points) [0 ..]
    compressed = [(xMap IM.! x, yMap IM.! y) | (x, y) <- points]
    decompressMap = M.fromList [((xMap IM.! x, yMap IM.! y), (x, y)) | (x, y) <- points]

greens :: CPoint -> [CPoint] -> Set CPoint
greens seed corners = inside
  where
    perim = S.fromList [(x, y) | ((x1, y1), (x2, y2)) <- perimeter corners, x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]
    neighbors (x, y) = S.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    fill seen queue
      | S.null queue = seen
      | otherwise =
          let (next, queue') = S.deleteFindMin queue
              seen' = S.insert next seen
              neighbs = neighbors next `S.difference` seen'
              queue'' = S.union queue' neighbs
           in fill seen' queue''
    inside = fill perim (S.singleton seed)

square :: (CPoint, CPoint) -> [CPoint]
square ((x1, y1), (x2, y2)) = do
  x <- [min x1 x2 .. max x1 x2]
  y <- [min y1 y2 .. max y1 y2]
  return (x, y)

part2 :: (MonadError String m) => PartSolution m
part2 = Solved $ \inp -> do
  points <- parseErr pInp inp
  let (compressed, mapping) = compress points
  let gs = greens (24, 98) compressed
  let validSquares = [corners | corners <- combos compressed, all (`S.member` gs) (square corners)]
  let decompressCorners = bimap (mapping !) (mapping !)
  let largest = maximum $ map (area . decompressCorners) validSquares
  return . T.show $ largest

day09 :: (MonadError String m) => Solution m
day09 = Solution part1 part2
