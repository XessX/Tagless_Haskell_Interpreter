data MDouble = M Double
    | VZ

data NInt = N Int
    | VZ2

data KInt = K Int
    | VZ3

data PString = P String
    | VZ4

data Exp = Lit Int
    | Neg Exp
    | Add Exp Exp
    | Sub Exp Exp    
    | Mul Exp Exp
    | Leq Exp Exp
    | Lt  Exp Exp
    | Neq Exp Exp
    | Eq  Exp Exp
    | Gt  Exp Exp
    | Gte Exp Exp
    | If_ Exp Exp Exp
    | Lam Exp Exp
    
len::Exp->Int
len(Lit n) = 1
len(Neg e) = 1 + len(e)
len(Add e1 e2) = 1 + len(e1) + len(e2)
len(Sub e1 e2) = 1 + len(e1) + len(e2)
len(Mul e1 e2) = 1 + len(e1) + len(e2)
len(Leq e1 e2) = 1 + len(e1) + len(e2)
len(Lt  e1 e2) = 1 + len(e1) + len(e2)
len(Neq e1 e2) = 1 + len(e1) + len(e2)
len(Eq  e1 e2) = 1 + len(e1) + len(e2)
len(Gt  e1 e2) = 1 + len(e1) + len(e2)
len(Gte e1 e2) = 1 + len(e1) + len(e2)
lan(Lam e1 e2) = 2 + len(e1) + len(e2)

view :: Exp->String
view(Lit n) = show n
view(Neg e) = "(-"++ view e ++")"
view(Add e1 e2) = "("++view e1 ++ "+" ++ view e2 ++")"
view(Sub e1 e2) = "("++view e1 ++ "-" ++ view e2 ++")"
view(Mul e1 e2) = "("++view e1 ++ "*" ++ view e2 ++")"
view(Leq e1 e2) = "("++view e1 ++ "<=" ++ view e2 ++")"
view(Lt e1 e2) = "("++view e1 ++  "<" ++ view e2 ++")"
view(Neq e1 e2) = "("++view e1 ++ "/=" ++ view e2 ++")"
view(Eq e1 e2) = "("++view e1 ++ "==" ++ view e2 ++")"
view(Gt e1 e2) = "("++view e1 ++ ">" ++ view e2 ++")"
view(Gte e1 e2) = "("++view e1 ++ ">=" ++ view e2 ++")"
view(Lam e1 e2) = "(\\x ->"++ view e1 ++ " x " ++ view e2 ++ ")"
view(If_ e1 e2 e3) = "if "++view e1 ++ " then " ++ view e2 ++ " else " ++ view e3

class ExpSYM repr where
  lit :: repr -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
  sub :: repr -> repr -> repr
  mul :: repr -> repr -> repr
  leq :: repr -> repr -> repr
  lt  :: repr -> repr -> repr
  neq :: repr -> repr -> repr
  eq  :: repr -> repr -> repr
  gt  :: repr -> repr -> repr
  gte :: repr -> repr -> repr
  if_ :: repr -> repr -> repr -> repr
  app :: (repr -> repr) -> repr -> repr
  lam :: (repr -> repr -> repr)  -> repr -> repr -> repr
  getInt :: repr -> Int
  getDouble :: repr -> Double

instance ExpSYM MDouble where
    lit (M x) = M x
    neg (M x) = (M (-x))
    add (M x) (M y) = M (x + y)
    sub (M x) (M y) = M (x - y)
    mul (M x) (M y) = M (x * y)
    leq (M x) (M y) = if x <= y then M (1.0) else M(0.0)
    lt  (M x) (M y) = if x <  y then M (1.0) else M(0.0)
    neq (M x) (M y) = if x /= y then M (1.0) else M(0.0)
    eq  (M x) (M y) = if x == y then M (1.0) else M(0.0)  
    gt  (M x) (M y) = if x >  y then M (1.0) else M(0.0)
    gte (M x) (M y) = if x >= y then M (1.0) else M(0.0)
    if_ (M x) (M y) (M z) = if (x == 1.0) then (M y) else (M z)
    lam fnc (M x) = (\y -> fnc y (M x))
    app fnc (M x) = fnc (M x)
    getInt (M x) = 0
    getDouble (M x) = x

instance ExpSYM NInt where
    lit (N x) = N x
    neg (N x) = (N (-x))
    add (N x) (N y) = N (x + y)
    sub (N x) (N y) = N (x - y)
    mul (N x) (N y) = N (x * y)
    leq (N x) (N y) = if x <= y then N (1) else N(0)
    lt  (N x) (N y) = if x <  y then N (1) else N(0)
    neq (N x) (N y) = if x /= y then N (1) else N(0)
    eq  (N x) (N y) = if x == y then N (1) else N(0)  
    gt  (N x) (N y) = if x >  y then N (1) else N(0)
    gte (N x) (N y) = if x >= y then N (1) else N(0)
    if_ (N x) (N y) (N z) = if (x == 1) then (N y) else (N z)
    lam fnc (N x) = (\y -> fnc y (N x))
    app fnc (N x) = fnc (N x)
    getInt (N x) = x
    getDouble (N x) = 0

instance ExpSYM KInt where
    lit (K x) = K 1
    neg (K x) = (K (1 + x))
    add (K x) (K y) = K (x + y + 1)
    sub (K x) (K y) = K (x + y + 1)
    mul (K x) (K y) = K (x + y + 1)
    leq (K x) (K y) = K (x + y + 1)
    lt  (K x) (K y) = K (x + y + 1)
    neq (K x) (K y) = K (x + y + 1)
    eq  (K x) (K y) = K (x + y + 1)  
    gt  (K x) (K y) = K (x + y + 1)
    gte (K x) (K y) = K (x + y + 1)
    if_ (K x) (K y) (K z) = K (x + y + 3)
    lam fnc (K x) (K y) = K (x + 3)
    app fnc (K x) = K (x + 1)
    getInt (K x) = x
    getDouble (K x) = 0


main = do  
    let k1 = add (lit (N 8)) (neg ( add (lit (N 1)) (lit (N 2))))
    let k2 = neg (k1)
    let k3 = lam add (N 2)
    let k4 = app k3 (k2)
    let k5 = if_ (eq k1 k2) (add (N 2) (N 5)) (mul (N 3) (k4))
    let f1 = add (lit (K 8)) (neg ( add (lit (K 1)) (lit (K 2))))
    let f2 = getInt(f1)
    let p1 = Add(Lit 8) (Neg (Add (Lit 1) (Lit 2)))
    let p2 = view p1
    let p3 = If_(Gt(Lit 2) (Lit 3))(Lit 5)(Lit 8)
    print(view p3)
    


