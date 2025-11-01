{-# LANGUAGE OverloadedStrings #-}

module SantaLib.Parsing
  ( module Text.Megaparsec,
    module Text.Megaparsec.Char,
    L.decimal,
    Parser,
    space,
    lexeme,
    lexemeLn,
    lexemeSp,
    symbol,
    symbolLn,
    symbolSp,
    filterNums,
    parseIO,
    signed,
    spaceLn,
    indented,
    aligned,
    L.nonIndented,
    lineSepNumbers,
  )
where

import Control.Monad (void)
import Data.Char (isNumber)
import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Void (Void)
import System.Exit (exitFailure)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- | My default space consumer, accepts only horizontal spaces, and uses // for
-- line comments, /* */ define a block comment
space :: Parser ()
space = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

spaceLn :: Parser ()
spaceLn = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- |  Parse lexeme and (horizontal) white space following it
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

-- | Parse lexeme and consume following eol
lexemeLn :: Parser a -> Parser a
lexemeLn = L.lexeme (void eol)

-- | Parse lexeme and one or zero following ' '
lexemeSp :: Parser a -> Parser a
lexemeSp = L.lexeme (void . optional . char $ ' ')

-- | Parse symbol and (horizontal) white space following it
symbol :: Text -> Parser Text
symbol = L.symbol space

-- | Parse symbol and consume following eol
symbolLn :: Text -> Parser Text
symbolLn = L.symbol (void eol)

-- | Parse symbol and one or zero following ' '
symbolSp :: Text -> Parser Text
symbolSp = L.symbol (void . optional . char $ ' ')

filterNums :: String -> [Int]
filterNums = map read . filter (all isNumber) . groupBy ((==) `on` isNumber)

signed :: (Num n) => Parser n -> Parser n
signed = L.signed space

parseIO :: Parser a -> FilePath -> Text -> IO a
parseIO parser path str = case parse parser path str of
  Left err -> putStrLn (errorBundlePretty err) >> exitFailure
  Right ok -> return ok

indented :: Pos -> Parser a -> Parser (Pos, a)
indented ref p = do
  pos <- L.indentGuard space GT ref
  v <- p
  return (pos, v)

aligned :: Pos -> Parser a -> Parser a
aligned ref p = L.indentGuard space EQ ref >> p

-- * Specific parsers for common patterns

lineSepNumbers :: (Num n) => Parser [n]
lineSepNumbers = some (L.lexeme (void eol <|> eof) L.decimal)
