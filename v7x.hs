-- Two data types are defined to support finally tagless style
import Data.Function
data MDouble = M Double
    | VZ

data NInt = N Int
    | VZ2

-- Expression is defined to include integer operations (neg, add, sub, mult), if then else, lambda expression and apllication and pair and projection:
data Exp = Lit Int
    | Neg Exp                   -- unary negation
    | Add Exp Exp               -- binary addition
    | Sub Exp Exp               -- binary subtraction
    | Mul Exp Exp               -- binary mutiplication
    | Addf                      -- Add function that can be used in lambda expressions
    | Subf                      -- Subtract function that can be used in lambda expressions
    | Mulf                      -- Multiplication function that can be used in lambda expressions
    | If_ BExp Exp Exp          -- if then else
    | Lam Exp Exp               -- lambda expression
    | Pair Exp Exp              -- pairs of two wxpressions
    | Fst Exp Exp               -- first of a pair
    | Snd Exp Exp               -- second of a pair
    | App Exp Exp               -- application operator

-- a datatype is defined for integer comparision and boolean expressions:
data BExp = Leq Exp Exp         -- <=
    | Lt  Exp Exp               -- <
    | Neq Exp Exp               -- /=
    | Eq  Exp Exp               -- ==
    | Gt  Exp Exp               -- >
    | Gte Exp Exp               -- >=
      
-- The len function computes the length if each expression based on the used operator and operand(s).
len::Exp->Int
len(Lit n) = 1
len(Neg e) = 1 + len(e)
len(Add e1 e2) = 1 + len(e1) + len(e2)
len(Sub e1 e2) = 1 + len(e1) + len(e2)
len(Mul e1 e2) = 1 + len(e1) + len(e2)
len(Addf) = 1
len(Subf) = 1
len(Mulf) = 1
len(Lam e1 e2) = 2 + len(e1) + len(e2)
len(App e1 e2) = len(e1) + len(e2)
len(If_ e1 e2 e3) = 3 + blen(e1) + len(e2) + len(e3)
len(Pair e1 e2) = len(e1) + len(e2) + 1
len(Fst e1 e2) = len(e1) + len(e2) + 1
len(Snd e1 e2) = len(e1) + len(e2) + 1

-- The blen function computes the length of a boolean expression.
blen::BExp->Int
blen(Leq e1 e2) = 1 + len(e1) + len(e2)
blen(Lt  e1 e2) = 1 + len(e1) + len(e2)
blen(Neq e1 e2) = 1 + len(e1) + len(e2)
blen(Eq  e1 e2) = 1 + len(e1) + len(e2)
blen(Gt  e1 e2) = 1 + len(e1) + len(e2)
blen(Gte e1 e2) = 1 + len(e1) + len(e2)

-- View function is defined for the pretty print
view :: Exp->String
view(Lit n) = show n
view(Neg e) = "(-"++ view e ++")"
view(Add e1 e2) = "("++view e1 ++ "+" ++ view e2 ++")"
view(Sub e1 e2) = "("++view e1 ++ "-" ++ view e2 ++")"
view(Mul e1 e2) = "("++view e1 ++ "*" ++ view e2 ++")"
view(Addf) = "Add"
view(Subf) = "Sub"
view(Mulf) = "Mul"
view(Lam e1 e2) = "(\\x ->"++ view e1 ++ " x " ++ view e2 ++ ")"
view(If_ e1 e2 e3) = "if "++bview e1 ++ " then " ++ view e2 ++ " else " ++ view e3
view(Pair e1 e2) = "pair("++view e1++" , "++view e2++")"
view(Fst e1 e2) = "fst("++view e1++" , "++view e2++")"
view(Snd e1 e2) = "snd("++view e1++" , "++view e2++")"
view(App e1 e2) = view e1 ++ " " ++ view e2

bview::BExp->String
bview(Leq e1 e2) = "("++view e1 ++ "<=" ++ view e2 ++")"
bview(Lt e1 e2) = "("++view e1 ++  "<" ++ view e2 ++")"
bview(Neq e1 e2) = "("++view e1 ++ "/=" ++ view e2 ++")"
bview(Eq e1 e2) = "("++view e1 ++ "==" ++ view e2 ++")"
bview(Gt e1 e2) = "("++view e1 ++ ">" ++ view e2 ++")"
bview(Gte e1 e2) = "("++view e1 ++ ">=" ++ view e2 ++")"

-- To evaluate each expression the following class is defioned:
class ExpSYM repr where
  lit :: repr -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
  sub :: repr -> repr -> repr
  mul :: repr -> repr -> repr
  leq :: repr -> repr -> Bool
  lt  :: repr -> repr -> Bool
  neq :: repr -> repr -> Bool
  eq  :: repr -> repr -> Bool
  gt  :: repr -> repr -> Bool
  gte :: repr -> repr -> Bool
  if_ :: Bool -> repr -> repr -> repr
  app :: (repr -> repr) -> repr -> repr
  lam :: (repr -> repr -> repr)  -> repr -> repr -> repr
  getInt :: repr -> Int
  getDouble :: repr -> Double
  pair :: repr -> repr -> (repr , repr)
  fst_ :: (repr , repr) -> repr
  snd_ :: (repr , repr) -> repr
  fix_ :: (repr -> repr) -> repr

-- An instance of the ExpSYM class for double numbers:
instance ExpSYM MDouble where
    lit (M x) = M x
    neg (M x) = (M (-x))
    add (M x) (M y) = M (x + y)
    sub (M x) (M y) = M (x - y)
    mul (M x) (M y) = M (x * y)
    leq (M x) (M y) = if x <= y then True else False
    lt  (M x) (M y) = if x <  y then True else False
    neq (M x) (M y) = if x /= y then True else False
    eq  (M x) (M y) = if x == y then True else False  
    gt  (M x) (M y) = if x >  y then True else False
    gte (M x) (M y) = if x >= y then True else False
    if_ (b) (M y) (M z) = if (b == True) then (M y) else (M z)
    lam fnc (M x) = (\y -> fnc y (M x))
    app fnc (M x) = fnc (M x)
    pair (M x) (M y) = (M x , M y)
    fst_ (M x , M y) = (M x)
    snd_ (M x , M y) = (M y)
    getInt (M x) = 0
    getDouble (M x) = x
    fix_ (fnc) = fix (fnc) 

-- An instance of the ExpSYM class for integer numbers:
instance ExpSYM NInt where
    lit (N x) = N x
    neg (N x) = (N (-x))
    add (N x) (N y) = N (x + y)
    sub (N x) (N y) = N (x - y)
    mul (N x) (N y) = N (x * y)
    leq (N x) (N y) = if x <= y then True else False
    lt  (N x) (N y) = if x <  y then True else False
    neq (N x) (N y) = if x /= y then True else False
    eq  (N x) (N y) = if x == y then True else False 
    gt  (N x) (N y) = if x >  y then True else False
    gte (N x) (N y) = if x >= y then True else False
    if_ (b) (N y) (N z) = if (b == True) then (N y) else (N z)
    lam fnc (N x) = (\y -> fnc y (N x))
    app fnc (N x) = fnc (N x)
    fst_ (N x , N y) = (N x)
    snd_ (N x , N y) = (N y)
    pair (N x) (N y) = (N x , N y)
    getInt (N x) = x
    getDouble (N x) = 0
    fix_ (fnc) = fix (fnc)     

main = do  
    -- testcase#1 evaluation:
    let k1 = add (lit (N 8)) (neg ( add (lit (N 1)) (lit (N 2))))
    let k2 = neg (k1)
    let k3 = lam add (N 2)
    let k4 = app k3 (k2)
    let k5 = if_ (eq k1 k2) (add (N 2) (N 5)) (mul(neg(N 4)) (N 6))
    let k6 = pair k4 k5
    print ("Result of evaluation: ")
    print(getInt (fst_ k6))
    
    -- testcase#2 prettyprint:    
    let p1 = Add(Lit 8) (Neg (Add (Lit 1) (Lit 2)))
    let p2 = Neg p1
    let p3 = Lam Addf (Lit 2)
    let p4 = App p3 (p2)
    let p5 = If_ (Eq p1 p2) (Add (Lit 2) (Lit 5)) (Mul (Neg (Lit 4)) (Lit 6))
    let p6 = Pair p4 p5
    let p7 = Fst p6 (Lit 2)
    let res = view p7
    print ("Pretty print: ")
    print(res)
    
    -- testcase#3 length of the script:
    let res2 = len p7
    print("length of the script: ");
    print(res2)
    --fix :: (a -> a) -> a
    --fix f = let {x = f x} in x
