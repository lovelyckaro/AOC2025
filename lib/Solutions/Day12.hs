module Solutions.Day12 where

import Control.Monad.Except
import Data.Text qualified as T
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

type Shape = [[Char]]

data Grid = Grid {size :: (Int, Int), wanted :: [Int]}
  deriving (Show, Eq, Ord)

pShape :: Parser Shape
pShape = do
  lexeme decimal
  symbol ":"
  eol
  some (lexemeLn (some $ oneOf ['.', '#']))

pGrid :: Parser Grid
pGrid = do
  x <- lexeme decimal
  symbol "x"
  y <- lexeme decimal
  let size = (x, y)
  symbol ":"
  wanted <- some (lexeme decimal)
  return Grid {..}

pInp :: Parser ([Shape], [Grid])
pInp = do
  shapes <- some (lexemeLn (try pShape))
  grids <- some (lexemeLn pGrid)
  return (shapes, grids)

largeEnough :: Grid -> Bool
largeEnough Grid {..} = squares >= needed
  where
    squares = uncurry (*) size
    needed = sum . map (* 9) $ wanted

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  (_, grids) <- parseErr pInp inp
  return . T.show . length . filter largeEnough $ grids

part2 :: (Monad m) => PartSolution m
part2 = Solved $ const (return "🎄 Merry christmas! 🎄")

day12 :: (MonadError String m) => Solution m
day12 = Solution part1 part2
