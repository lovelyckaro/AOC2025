module Solutions.Day10 where

import Algorithm.Search (dijkstra)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bits
import Data.SBV
import Data.Text qualified as T
import Data.Word
import SantaLib hiding (part1, part2)
import SantaLib.Parsing

data Machine = Machine {lights :: Word16, buttons :: [[Int]], joltageRequirements :: [Integer]}
  deriving (Show, Eq, Ord)

fromBools :: (Bits a) => [Bool] -> a
fromBools xs = foldr (\(n, bool) acc -> if bool then acc `setBit` n else acc) zeroBits (zip [0 ..] xs)

pLights :: Parser Word16
pLights = lexeme $ char '[' *> (fromBools <$> many pLight) <* char ']'
  where
    pLight = True <$ char '#' <|> False <$ char '.'

pButtons :: Parser [[Int]]
pButtons = many $ lexeme pButton
  where
    pButton = char '(' *> (decimal `sepBy` char ',') <* char ')'

pJoltage :: Parser [Integer]
pJoltage = lexeme $ char '{' *> decimal `sepBy` char ',' <* char '}'

pMachine :: Parser Machine
pMachine = do
  lights <- pLights
  buttons <- pButtons
  joltageRequirements <- pJoltage
  return $ Machine {..}

pInp :: Parser [Machine]
pInp = many $ lexemeLn pMachine

fewestPresses :: Machine -> Int
fewestPresses (Machine desired buttons _) = cost
  where
    neighbor button lights = foldr (\n acc -> acc `complementBit` n) lights button
    neighbors n = [neighbor button n | button <- buttons]
    Just (cost, _path) = dijkstra neighbors (\_ _ -> 1) (== desired) zeroBits

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  machines <- parseErr pInp inp
  let fewest = map fewestPresses machines
  return . T.show $ sum fewest

formulateProblem :: Machine -> Symbolic ()
formulateProblem (Machine _ buttons desired) = do
  buttonPresses <- zip buttons <$> sIntegers ["presses_" <> show n | n <- [0 .. length buttons - 1]]
  mapM_ (constrain . (.>= 0) . snd) $ buttonPresses
  forM_ (zip desired [0 ..]) $ \(wantedJoltage, n) -> do
    let relevantButtons = [presses | (buttonNums, presses) <- buttonPresses, n `elem` buttonNums]
    constrain (fromInteger wantedJoltage .== sum relevantButtons)
  minimize "total" $ sum (map snd buttonPresses)

solveJoltage :: (MonadIO m) => Machine -> m Integer
solveJoltage m = liftIO $ do
  let problem = formulateProblem m
  LexicographicResult res <- optimize Lexicographic problem
  let Just cost = getModelValue @_ @Integer "total" res
  return cost

part2 :: (MonadError String m, MonadIO m) => PartSolution m
part2 = Solved $ \inp -> do
  machines <- parseErr pInp inp
  costs <- mapM solveJoltage machines
  return . T.show $ sum costs

day10 :: (MonadError String m, MonadIO m) => Solution m
day10 = Solution part1 part2
