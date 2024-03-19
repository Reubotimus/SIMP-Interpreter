module SimpParser (parseProgram, parse) where
import ParserModule
import Grammar
import Control.Applicative
import Data.Char
import System.Environment (getArgs)

parseProgram = whitespace *> (C <$> parseCommand) <|> 
               (B <$> parseBooleanExpression) <|> 
               (E <$> parseExpression) <* whitespace

-- Elementary parsers
whitespace :: Parser String
whitespace = many (char ' ' <|> 
                   char '\n' <|> 
                   char '\t' <|> 
                   char '\v' <|> 
                   char '\f' <|> 
                   char '\r')
parseIntOperation = (string "+" *> pure ((+))) <|> 
                    (string "-" *> pure ((-))) <|> 
                    (string "*" *> pure ((*))) <|> 
                    (string "%" *> pure (mod)) <|> 
                    (string "/" *> pure ((div)))
parseBinaryOperation = (string ">=" *> pure ((\x y -> Bin (x >= y)))) <|>
                       (string "<=" *> pure ((\x y -> Bin (x <= y)))) <|> 
                       (string "<" *> pure (\x y -> Bin (x < y))) <|> 
                       (string ">" *> pure ((\x y -> Bin (x > y)))) <|>
                       (string "=" *> pure ((\x y -> Bin (x == y))))
parseSemiColon = do
    whitespace
    char ';'
    whitespace

-- Expression Parsers
parseN = (N . fromIntegral) <$> int
parseVariable = some alpha
parseDereference = char '!' *> (Dereference <$> parseVariable)
parseOp = do
    expr1 <- parseNonRecursiveExpression
    whitespace
    op <- parseIntOperation
    whitespace
    expr2 <- parseExpression
    return (Op op expr1 expr2)
parseExpressionBracket = do
    char '('
    whitespace
    c <- parseExpression
    whitespace
    char ')'
    return c
parseNonRecursiveExpression = parseDereference <|> parseN <|> parseExpressionBracket
parseExpression = parseOp <|> parseNonRecursiveExpression

-- Boolean Expression Parsers
parseBool = (string "true" *> pure (Bin True)) <|> 
            (string "false" *> pure (Bin False))
parseNot = Not <$> (char '~' *> whitespace *> (parseNonRecursiveBoolean))
parseAnd = do
    b1 <- parseNonRecursiveBoolean
    whitespace
    string "&&"
    whitespace
    b2 <- parseNonRecursiveBoolean
    return (And b1 b2)
parseBooleanOperation = do
    e1 <- parseNonRecursiveExpression
    whitespace
    op <- parseBinaryOperation
    whitespace
    e2 <- parseNonRecursiveExpression
    return (Bop op e1 e2)
parseBooleanBracket = do 
    char '('
    whitespace
    c <- parseBooleanExpression
    whitespace
    char ')'
    return c
parseNonRecursiveBoolean = parseBool <|> parseBooleanBracket <|> parseNot
parseBooleanExpression = parseBooleanOperation <|> parseAnd <|> parseNonRecursiveBoolean

-- Command parsers
parseSkip = string "skip" *> pure Skip
parseAssign = do
    v <- parseVariable
    whitespace
    string ":="
    whitespace
    e <- parseExpression
    return (Assign v e)
parseIf = do
    string "if"
    whitespace
    b <- parseBooleanBracket
    whitespace
    string "then"
    whitespace
    c1 <- parseCommandBracket
    whitespace
    string "else"
    whitespace
    c2 <- parseCommandBracket
    return (If b c1 c2)
parseWhile = do
    string "while"
    whitespace
    b <- parseBooleanBracket
    whitespace
    string "do"
    whitespace
    c <- parseCommandBracket
    return (While b c)
parseCommandBracket = do 
    char '('
    whitespace
    c <- parseCommand
    whitespace
    char ')'
    return c
parseThen = (foldr1 Then) <$> (sepBy1 parseNonRecursiveCommand parseSemiColon)
parseCommand = whitespace *> (
        parseThen <|> 
        parseNonRecursiveCommand <|> 
        parseNonRecursiveCommand
    ) <* whitespace <* (many (char ';')) <* whitespace
parseNonRecursiveCommand = 
    whitespace *> (parseSkip <|> 
    parseAssign <|> 
    parseIf <|> 
    parseWhile <|> 
    parseCommandBracket) <* whitespace

-- for testing purposes
main = do
    args <- getArgs
    content <- readFile (args !! 0)
    putStrLn $ show $ parse parseProgram content