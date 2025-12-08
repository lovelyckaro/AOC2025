module Solutions.Day06 where

import Control.Monad.Error.Class (MonadError)
import Data.Char (isSpace)
import Data.List (sort, transpose)
import Data.List.Split (splitWhen)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Log (MonadLog, logTrace)
import SantaLib hiding (part1, part2)
import SantaLib.Parsing hiding (Token, lexemeLn, lexemeSp)
import Text.Pretty.Simple (pPrint)

data Token = Number !Int | Add | Mul
  deriving (Show, Eq)

instance Ord Token where
  compare Add Add = EQ
  compare Add _ = LT
  compare Mul Add = GT
  compare Mul Mul = EQ
  compare Mul (Number _) = LT
  compare (Number x) (Number y) = compare x y
  compare (Number _) _ = GT

pToken :: Parser Token
pToken = (Mul <$ char '*') <|> (Add <$ char '+') <|> (Number <$> decimal)

pRow :: Parser [Token]
pRow = do
  space
  tokens <- many (lexeme pToken)
  eol
  return tokens

pInp :: Parser [[Token]]
pInp = do
  space
  rows <- many pRow
  eof
  let cols = transpose $ reverse rows
  return cols

eval :: [Token] -> Int
eval [] = 0
eval (Mul : nums) = product [num | Number num <- nums]
eval (Add : nums) = sum [num | Number num <- nums]

part1 :: (MonadError String m) => PartSolution m
part1 = Solved $ \inp -> do
  cols <- parseErr pInp inp
  return . T.show . sum . map eval $ cols

pLine :: Parser [Token]
pLine = do
  space
  token <- lexeme pToken
  mbToken <- lexeme (optional pToken)
  eol
  return $ catMaybes [Just token, mbToken]

pChunk :: Parser [Token]
pChunk = do
  lines <- many pLine
  return . sort . concat $ lines

toChunks :: Text -> [Text]
toChunks = map T.unlines . splitWhen (T.all isSpace) . T.transpose . T.lines

part2 :: (MonadError String m, MonadLog m) => PartSolution m
part2 = Solved $ \inp -> do
  let chunks = toChunks inp
  tokens <- mapM (parseErr pChunk) chunks
  logTrace "tokens" (show tokens)
  return . T.show . sum . map eval $ tokens

day06 :: (MonadError String m, MonadLog m) => Solution m
day06 = Solution part1 part2
