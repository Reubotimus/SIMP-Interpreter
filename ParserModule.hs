module ParserModule where

import Data.Maybe
import Control.Applicative
import Data.Char


newtype Parser a = P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
    -- fmap :: (a -> b) -> P a -> P b
    fmap f (P x) =  P $ \s -> do
        (a, s') <- x s
        return (f a, s')
    

instance Applicative Parser where
    -- pure :: a -> P a
    pure a = P (\x -> Just (a, x))
    -- (<*>) :: f (a -> b) -> f a -> f b
    (P p1) <*> (P p2) = P $ \s -> do
        (f, s')  <- p1 s
        (a, s'') <- p2 s'
        return (f a, s'')
    
instance Monad Parser where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (P p) >>= f = P $ \s -> do
        (a, s') <- p s
        parse (f a) s'

instance Alternative Parser where
    empty = P $ \_ -> Nothing
    (P p1) <|> (P p2) = P $ \s -> p1 s <|> p2 s

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy elementParser separatorParser = sepBy1 elementParser separatorParser <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 elementParser separatorParser = do
  first <- elementParser
  rest <- many (separatorParser >> elementParser)
  return (first:rest)

char :: Char -> Parser Char
char c = P (\s -> do
                    x <- listToMaybe s
                    if x == c then return (c, tail s) else Nothing)
alpha = P (\s -> do
                    x <- listToMaybe s
                    if isAlpha x then return (x, tail s) else Nothing)

string :: String -> Parser String
string = mapM char

int' = (char '0' <|> char '1' <|> char '2' 
        <|> char '3' <|> char '4' <|> char '5' 
        <|> char '6' <|> char '7' <|> char '8' 
        <|> char '9')

readInt x = read x :: Integer
int = fmap readInt (some int')

readDbl x = read x :: Double
dbl = readDbl <$> 
    ((++) <$> ((++) <$> (some int') <*> (string ".")) <*> (some int'))