{-# LANGUAGE LambdaCase #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative
import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Parser a = Parser {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap f' . p)
    where
      f' (a, str) = (f a, str)

instance Applicative Parser where
  pure a = Parser (\input -> Just (a, input))
  (Parser f) <*> (Parser p) =
    Parser $ \input -> do
      (f', input') <- f input
      (a, s'') <- p input'
      return (f' a, s'')

instance Alternative Parser where
  empty = Parser (const Nothing)
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

instance Monad Parser where
  (Parser p) >>= f =
    Parser $ \input -> do
      (a, input') <- p input
      let (Parser p') = f a
      p' input'

failedParser :: Parser a
failedParser = Parser $ \_ -> Nothing

oneChar :: Parser Char
oneChar =
  Parser
    ( \case
        [] -> Nothing
        (c : cs) -> Just (c, cs)
    )

satP :: (Char -> Bool) -> Parser Char
satP f = do
  a <- oneChar
  if f a
    then return a
    else failedParser

alphaChar :: Parser Char
alphaChar = satP isAlpha

digitChar :: Parser Char
digitChar = satP isDigit

char :: Char -> Parser Char
char c = satP (== c)

string :: String -> Parser String
string = traverse char

intOp = plus <|> minus <|> times <|> divide
  where
    plus = char '+' >> return (+)
    minus = char '-' >> return (-)
    times = char '*' >> return (*)
    divide = char '/' >> return div

oneNat :: Parser Int
oneNat = read <$> some digitChar

addOp :: Parser (Int -> Int -> Int)
addOp = plus <|> minus
  where
    plus = char '+' >> return (+)
    minus = char '-' >> return (-)

mulOp :: Parser (Int -> Int -> Int)
mulOp = plus <|> minus
  where
    plus = char '*' >> return (*)
    minus = char '/' >> return div

-- sumE1 :: Parser Int
-- sumE1 = prodE1 >>= rest where
--         rest x = next x <|> return x
--         next x = do
--             o <- addOp
--             y <- prodE1
--             rest $ x `o` y

-- prodE1 :: Parser Int
-- prodE1 = factorE1 >>= rest where
--         rest x = next x <|> return x
--         next x = do
--             o <- mulOp
--             y <- factorE1
--             rest $ x `o` y

-- factorE1 :: Parser Int
-- factorE1 = parenE <|> oneNat where
--            parenE = do
--                char '('
--                n <- sumE1
--                char ')'
--                return n


chainl :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl p pop = p >>= rest
  where
    rest x = next x <|> return x
    next x = do
      o <- pop
      y <- p
      rest $ x `o` y

between :: Char -> Parser b -> Char -> Parser b
between l p r = do
  char l
  x <- p
  char r
  return x

sumE :: Parser Int
sumE = prodE `chainl` addOp

prodE :: Parser Int
prodE = factorE `chainl` mulOp

factorE :: Parser Int
factorE = between '(' sumE ')' <|> oneNat 
