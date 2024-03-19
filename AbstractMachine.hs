module AbstractMachine (interpret, Token(TP), eval, State) where
import Grammar
import Data.Map (Map, insert, (!))
import qualified Data.Map as Map

data Token 
    = TP Program 
    | TC Command 
    | TE Expression 
    | TB Boolean
    | TNOT
    | TAND
    | TOP IntOperation
    | TBOP BinaryOperation
    | TASS
    | TVAR Variable
    | TIF
    | TWHILE
    deriving (Show)

type State = ([Token], [Token], Map String Int)


get :: Variable -> Map String Int -> Int
get var memory = memory ! var

eval :: State -> State

eval ((TP (E e) : cs), r, m) = ((TE e : cs), r, m)
eval ((TP (C e) : cs), r, m) = ((TC e : cs), r, m)
eval ((TP (B e) : cs), r, m) = ((TB e : cs), r, m)

eval ((TE (N n) : cs), r, m)                            = (cs, (TE (N n) : r), m)
eval ((TE (Op op e1 e2) : cs), r, m)                    = ((TE e2 : TE e1 : TOP op : cs), r, m)
eval ((TOP op : cs), (TE (N n1) : TE (N n2) : rs), m)   = (cs, (TE (N (op n1 n2)) : rs), m)

eval ((TB (Bin b) : cs), r, m)                          = (cs, (TB (Bin b) : r), m)
eval ((TB (Not bool) : cs), r, m)                       = ((TB bool : TNOT : cs), r, m)
eval ((TB (And b1 b2) : cs), r, m)                      = ((TB b1 : TB b2 : TAND : cs), r, m)
eval ((TNOT : cs), (TB (Bin b) : rs), m)                = (cs, (TB (Bin (not b)) : rs), m)
eval ((TAND : cs), (TB (Bin b1) : TB (Bin b2) : rs), m) = (cs, (TB (Bin (b1 && b2)) : rs), m)
eval ((TB (Bop op e1 e2) : cs), r, m)                   = ((TE e1 : TE e2 : TBOP op : cs), r, m)
eval ((TBOP op : cs), (TE (N n2) : TE (N n1) : rs), m)  = (cs, (TB (op n1 n2) : rs), m)
eval ((TE (Dereference v) : cs), r, m)                  = (cs, (TE (N n) : r), m)
    where
        n = get v m

eval ((TC Skip : cs), r, m)                     = (cs, r, m)

eval (TC (Assign l e) : cs, rs, m)              = ((TE e : TASS : cs), (TVAR l : rs), m)
eval ((TASS : cs), (TE (N n) : TVAR v : rs), m) = (cs, rs, insert v n m)

eval ((TC (If b c1 c2) : cs), rs, m) = ((TB b : TIF : cs), (TC c1 : TC c2 : rs), m)
eval ((TIF : cs), (TB (Bin b) : c1 : c2 : rs), m)
    | b == True = ((c1 : cs), rs, m)
    | b == False = ((c2 : cs), rs, m)

eval ((TC (While b c) : cs), rs, m) = ((TB b : TWHILE : cs), (TB b: TC c : rs), m)
eval ((TWHILE : cs), (TB (Bin b) : TB boolexp : TC c : rs), m)
    | b == True = ((TC c : TC (While boolexp c) : cs), rs, m)
    | b == False = (cs, rs, m)

eval (TC (Then c1 c2) : cs, rs, m) = (TC c1 : TC c2 : cs, rs, m)
eval _ = error "Invalid transition"
interpret :: State -> State
interpret ([], r, m) = ([], r, m)
interpret s = interpret (eval s)


