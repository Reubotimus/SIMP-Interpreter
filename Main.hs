import SimpParser
import Grammar
import AbstractMachine
import System.Environment (getArgs)
import Data.Map (empty)

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let parseResult = parse parseProgram content
    putStrLn $ show parseResult
    let syntaxTree = case parseResult of
            Just (tree, "") -> tree
            Just _          -> error "not able to fully parse"
            Nothing         -> error "parsing error"
    -- eval' ([TP $ syntaxTree], [], empty)
    putStrLn $ show $ interpret ([TP $ syntaxTree], [], empty)

-- For debugging purposes, prints the memory for every transition in the abstract machine
eval' :: State -> IO ()
eval' ([], r, m) = do
    putStrLn $ show m
eval' state@(_,_,m) = do
    putStrLn $ show m
    eval' $ eval state