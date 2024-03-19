module Grammar where

data Program = C Command | E Expression | B Boolean
    deriving (Show)

data Command 
    = Skip 
    | Assign Variable Expression 
    | Then Command Command
    | If Boolean Command Command
    | While Boolean Command
    deriving (Show)

data Expression 
    = Dereference Variable 
    | N Int 
    | Op IntOperation Expression Expression
    deriving (Show)

type IntOperation = Int -> Int -> Int
instance Show IntOperation where
    show operation = "intOperation"

data Boolean
    = Bin Bool
    | Bop BinaryOperation Expression Expression
    | Not Boolean
    | And Boolean Boolean
    deriving (Show)

type BinaryOperation = Int -> Int -> Boolean
instance Show BinaryOperation where
    show operation = "boolOperation"

type Variable = String
