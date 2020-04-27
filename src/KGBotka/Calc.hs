{-# LANGUAGE OverloadedStrings #-}
module KGBotka.Calc where

import KGBotka.Parser
import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Data.Char (isDigit, isAlpha)
import qualified Data.Text as T

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  deriving (Show, Eq)

charToOperator :: Char -> Operator
charToOperator '+' = Add
charToOperator '-' = Sub
charToOperator '*' = Mul
charToOperator '/' = Div
charToOperator '%' = Mod
charToOperator '^' = Pow
charToOperator _ =
  error
    "charToOperator: No pattern for operator conversion. \
    \Maybe you added a new operator and forgot to add a case to the conversion function."

data CalcExpression
  = BinaryExpression Operator CalcExpression CalcExpression
  | NegativeExpression CalcExpression
  | FunctionApplication T.Text [CalcExpression]
  | ValueExpression Double
  deriving (Show)

hasChar :: Char -> Parser Bool
hasChar c =
  Parser $ \input ->
    case T.uncons input of
      Just (c', rest) | c' == c -> Right (rest, True)
      _ -> Right (input, False)

parseNumber :: Parser Double
parseNumber = parseFloating <|> parseInteger
  where
    parseNumeric :: Parser T.Text
    parseNumeric = notNull "Expected a numeric value" $ takeWhileP isDigit
    parseInteger :: Parser Double
    parseInteger = read . T.unpack <$> parseNumeric
    parseFloating :: Parser Double
    parseFloating = do
      integerPart <- parseNumeric
      void $ charP '.'
      fractionalPart <- parseNumeric
      return $ read $ T.unpack $ integerPart <> "." <> fractionalPart

parseLine :: Parser CalcExpression
parseLine = parseExpression <* eof

parseExpression :: Parser CalcExpression
parseExpression = parseAdditive

parseAdditive :: Parser CalcExpression
parseAdditive = parseAdditive' <|> parseMultiplicative
  where
    parseAdditive' = do
      left <- parseMultiplicative
      operator <- charToOperator <$> (charP '+' <|> charP '-')
      BinaryExpression operator left <$> parseAdditive

parseMultiplicative :: Parser CalcExpression
parseMultiplicative = parseMultiplicative' <|> parseExponentiation
  where
    parseMultiplicative' = do
      left <- parseExponentiation
      operator <- charToOperator <$> (charP '*' <|> charP '/' <|> charP '%')
      BinaryExpression operator left <$> parseMultiplicative

parseExponentiation :: Parser CalcExpression
parseExponentiation = parseExponentiation' <|> parseAtom
  where
    parseExponentiation' = do
      left <- parseNegation
      operator <- charToOperator <$> charP '^'
      BinaryExpression operator left <$> parseExponentiation

parseNegation :: Parser CalcExpression
parseNegation = parseNegation' <|> parseAtom
  where
    parseNegation' = do
      void $ charP '-'
      NegativeExpression <$> parseNegation

parseFunctionApplication :: Parser CalcExpression
parseFunctionApplication = do
  functionName <- notNull "Expected a function name" $ takeWhileP isAlpha
  FunctionApplication functionName <$>
    inParens (sepBy parseExpression (charP ',' <* ws) <|> return [])

parseValue :: Parser CalcExpression
parseValue = ValueExpression <$> parseNumber

parseAtom :: Parser CalcExpression
parseAtom =
  ws *> (parseValue <|> parseFunctionApplication <|> inParens parseExpression) <*
  ws
