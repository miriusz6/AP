
data Exp = Cst Integer
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp
    | Pow Exp Exp
    | If {test, yes, no :: Exp}
    | Var VName
    | Let {var :: VName, def, body :: Exp}
    | Sum {var :: VName, from, to, body :: Exp}

type VName = String

showExp :: Exp -> String
showExp (Cst val) = show val 
showExp (Add a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")" 
showExp (Sub a b) = "(" ++ showExp a ++ "-" ++ showExp b ++ ")" 
showExp (Mul a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")" 
showExp (Div a b) = "(" ++ showExp a ++ "'div'" ++ showExp b ++ ")"
showExp (Pow a b) = "(" ++ showExp a ++ "^" ++ showExp b ++ ")" 
-- showExp _ = ERROR INVOKE

evalSimple :: Exp -> Integer
evalSimple (Cst val) = val 
evalSimple (Add a b) = evalSimple a + evalSimple b
evalSimple (Sub a b) = evalSimple a - evalSimple b
evalSimple (Mul a b) = evalSimple a * evalSimple b
evalSimple (Div a b) = div (evalSimple a) (evalSimple b)
evalSimple (Pow a b) = evalSimple a ^ evalSimple b
evalSimple (If t y n)
    | evalSimple t == 0 = evalSimple n
    | otherwise = evalSimple y

--toInteger $ 

a = (Add (Cst 1) (Cst 2))

c = Cst 1

sum'' = (Add (Cst 1) (Cst 2))
sum' = showExp sum''

sub'' = (Sub (Cst 1) (Cst 2))
sub' = showExp sub''

mul'' = (Mul (Cst 1) (Cst 2))
mul' = showExp mul''

div'' = (Div (Cst 4) (Cst 2))
div' = showExp div''

pow'' = (Pow (Cst 3) (Cst 0))
pow' = showExp pow''

if'' = (If (Add (Cst 1) (Cst 1)) (Cst 5) (Cst 10))

res = sum' ++ "\n" ++ sub' ++ "\n" ++ mul' ++ "\n" ++ div' ++ "\n" ++ pow'

resAd = Add sum'' mul''

simpleResult = putStrLn res

result = putStrLn $ showExp resAd

