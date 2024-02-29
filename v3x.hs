
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
    lam ff (N x) = (\y -> ff y (N x))
    app fnc (N x) = fnc (N x)
    getInt (N x) = x
    getDouble (N x) = 0

main = do  
    let k1 = add (lit (N 8)) (neg ( add (lit (N 1)) (lit (N 2))))
    let k2 = neg (k1)
    let k3 = lam add (N 2)
    let k4 = app k3 (k2)
    print (getInt k4)



