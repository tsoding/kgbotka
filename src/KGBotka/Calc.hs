{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module KGBotka.Calc where

import Control.Applicative (Alternative(..))
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Data.Char (isAlpha, isDigit)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import KGBotka.Parser
import System.Random (randomIO)

data Operator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  deriving (Show, Eq)

data CalcExpression
  = BinaryExpression Operator CalcExpression CalcExpression
  | NegativeExpression CalcExpression
  | FunctionApplication T.Text [CalcExpression]
  | ValueExpression Double
  deriving (Show)

newtype CalcEvalError =
  CalcEvalError T.Text
  deriving (Show)

type CalcEval = ExceptT CalcEvalError IO

parseNumber :: Parser Double
parseNumber = parseFloating <|> parseInteger
  where
    parseNumeric :: Parser T.Text
    parseNumeric = notNull "Expected a numeric value" $ takeWhileP isDigit
    parseInteger :: Parser Double
    parseInteger = read . T.unpack <$> parseNumeric
    -- TODO(#177): parseFloating does not support exponential number format
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
      operator <- (Add <$ charP '+') <|> (Sub <$ charP '-')
      BinaryExpression operator left <$> parseAdditive

parseMultiplicative :: Parser CalcExpression
parseMultiplicative = parseMultiplicative' <|> parseExponentiation
  where
    parseMultiplicative' = do
      left <- parseExponentiation
      operator <-
        (Mul <$ charP '*') <|> (Div <$ charP '/') <|> (Mod <$ charP '%')
      BinaryExpression operator left <$> parseMultiplicative

parseExponentiation :: Parser CalcExpression
parseExponentiation = parseExponentiation' <|> parseAtom
  where
    parseExponentiation' = do
      left <- parseNegation
      operator <- Pow <$ charP '^'
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

-- TODO(#178): Make calc variables a seperate constructor of CalcExpression
parseVariable :: Parser CalcExpression
parseVariable = do
  varName <- notNull "Expected a variable name" $ takeWhileP isAlpha
  return $ FunctionApplication varName []

parseValue :: Parser CalcExpression
parseValue = ValueExpression <$> parseNumber

parseAtom :: Parser CalcExpression
parseAtom =
  ws *>
  (parseValue <|> parseFunctionApplication <|> parseVariable <|>
   inParens parseExpression) <*
  ws

evalCalcExpression :: CalcExpression -> CalcEval Double
evalCalcExpression (BinaryExpression op left right) = do
  left' <- evalCalcExpression left
  right' <- evalCalcExpression right
  return $
    (case op of
       Add -> (+)
       Sub -> (-)
       Mul -> (*)
       Div -> (/)
       Mod ->
         \a b ->
           fromIntegral $
           mod (toInteger (floor a :: Integer)) (toInteger (floor b :: Integer))
       Pow -> (**))
      left'
      right'
evalCalcExpression (NegativeExpression body) =
  (* (-1.0)) <$> evalCalcExpression body
evalCalcExpression (ValueExpression val) = return val
evalCalcExpression (FunctionApplication functionName args) =
  case M.lookup functionName functionLookupTable of
    Just f -> mapM evalCalcExpression args >>= f
    Nothing -> throwE $ CalcEvalError "undefined is not a function FeelsDankMan"

functionLookupTable :: M.Map T.Text ([Double] -> CalcEval Double)
functionLookupTable =
  M.fromList
    [ ( "pi"
      , \case
          [] -> return pi
          _ -> throwE $ CalcEvalError "pi is not a function")
    , ( "e"
      , \case
          [] -> return $ exp 1
          _ -> throwE $ CalcEvalError "e is not a function")
    , ( "sin"
      , \case
          [x] -> return $ sin x
          _ -> throwE $ CalcEvalError "sin expects one argument")
    , ( "cos"
      , \case
          [x] -> return $ cos x
          _ -> throwE $ CalcEvalError "cos expects one argument")
    , ( "tan"
      , \case
          [x] -> return $ tan x
          _ -> throwE $ CalcEvalError "tan expects one argument")
    , ( "arcsin"
      , \case
          [x] -> return $ asin x
          _ -> throwE $ CalcEvalError "arcsin expects one argument")
    , ( "arccos"
      , \case
          [x] -> return $ acos x
          _ -> throwE $ CalcEvalError "arccos expects one argument")
    , ( "arctan"
      , \case
          [x] -> return $ atan x
          _ -> throwE $ CalcEvalError "arctan expects one argument")
    , ( "exp"
      , \case
          [x] -> return $ exp x
          _ -> throwE $ CalcEvalError "exp expects one argument")
    , ( "ln"
      , \case
          [x] -> return $ log x
          _ -> throwE $ CalcEvalError "ln expects one argument")
    , ( "nthroot"
      , \case
          [n, x] -> return $ x ** recip n
          _ ->
            throwE $
            CalcEvalError "nthroot expects two arguments (radix and radicand)")
    , ( "sqrt"
      , \case
          [x] -> return $ sqrt x
          _ -> throwE $ CalcEvalError "sqrt expects one argument")
    , ( "random"
      , \case
          [] -> lift randomIO
          _ -> throwE $ CalcEvalError "random takes no arguments")
    ]
