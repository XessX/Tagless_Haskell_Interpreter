
data Exp = Lit Int
     | Neg Exp
     | Add Exp Exp
     | Mul Exp Exp
     | Sub Exp Exp
     | If BoolExp Exp Exp


data BoolExp = Lt Exp Exp
     | Eq Exp Exp
     | Leq Exp Exp
     | Gt Exp Exp
     | Geq Exp Exp
     
eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e) = -eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (If be e1 e2) = if beval be then eval e1 else eval e2

beval :: BoolExp -> Bool
beval (Lt e1 e2) = (eval e1) < (eval e2)
beval (Eq e1 e2) = (eval e1) == (eval e2)
beval (Leq e1 e2) = (eval e1) <= (eval e2)
beval (Gt e1 e2) = eval e1 > eval e2
beval (Geq e1 e2) = eval e1 >= eval e2

first :: (a, b, c) -> a
first (x, _, _) = x

main = do  
    let tpl = (23,6,5)
    let ti1 = Lit 8
    let ti2 = Lit 12
    let b1 = Eq ti1 (Add ti2 (Lit 3))
    print (beval b1)
    



