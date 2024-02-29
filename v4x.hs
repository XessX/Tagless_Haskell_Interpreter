data MDouble = M Double
    | VZ
data NInt = N Int
    | VZ2

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
    --neq (M x) (M y) = if x != y then M (1.0) else M(0.0)
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
    --neq (M x) (M y) = if x != y then M (1.0) else M(0.0)
    eq  (N x) (N y) = if x == y then N (1) else N(0)  
    gt  (N x) (N y) = if x >  y then N (1) else N(0)
    gte (N x) (N y) = if x >= y then N (1) else N(0)
    if_ (N x) (N y) (N z) = if (x == 1) then (N y) else (N z)
    lam fnc (N x) = (\y -> fnc y (N x))
    app fnc (N x) = fnc (N x)
    getInt (N x) = x
    getDouble (N x) = 0

main = do  
    let k1 = add (lit (N 8)) (neg ( add (lit (N 1)) (lit (N 2))))
    let k2 = neg (k1)
    let k3 = lam add (N 2)
    let k4 = app k3 (k2)
    let k5 = if_ (eq k1 k2) (add (N 2) (N 5)) (mul (N 3) (k4))
    print (getInt k5)



