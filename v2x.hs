
class ExpSYM repr where
  lit :: repr -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr
  sub :: repr -> repr -> repr
  mul :: repr -> repr -> repr
  getInt :: repr -> Int
  getDouble :: repr -> Double

data MDouble = M Double
data NInt = N Int


instance ExpSYM MDouble where
    lit (M x) = M x
    neg (M x) = (M (-x))
    add (M x) (M y) = M (x + y)
    sub (M x) (M y) = M (x - y)
    mul (M x) (M y) = M (x * y)
    getInt (M x) = 0
    getDouble (M x) = x

instance ExpSYM NInt where
    lit (N x) = N x
    neg (N x) = (N (-x))
    add (N x) (N y) = N (x + y)
    sub (N x) (N y) = N (x - y)
    mul (N x) (N y) = N (x * y)        
    getInt (N x) = x
    getDouble (N x) = 0

instance ExpSYM Double where
    lit x =  x
    neg (x) = -x
    add x y = x + y
    sub x y = x - y
    mul x y = x * y
    getInt ( x) = 0
    getDouble ( x) = x

main = do  
    let k1 = add (lit (N 8)) (neg ( add (lit (N 1)) (lit (N 2))))
    let k2 = neg (k1)
    print (getInt k1)



