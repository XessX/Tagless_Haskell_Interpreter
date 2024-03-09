module R where

import TypesClasses

-- An R interpreter that executes expressions
newtype R a = R { unR :: a } 
deriving instance Eq a => Eq (R a)

instance ExpSYM R where
    int x = R x
    bool b = R b
    float f = R f
    add (R x) (R y) = R (x + y)
    neg (R x) = R (negate x)
    sub (R x) (R y) = R (x - y)
    mul (R x) (R y) = R (x * y)
    leq (R x) (R y) = R (x <= y)
    lt (R x) (R y) = R (x < y)
    neq (R x) (R y) = R (x /= y)
    eq (R x) (R y) = R (x == y)
    gt (R x) (R y) = R (x > y)
    gte (R x) (R y) = R (x >= y)
    if_ (R b) (R x) (R y) = R (if b then x else y)
    lam f = R (\x -> unR (f (R x)))
    app (R f) (R x) = R (f x)
    fix f = R (let {x = unR (f (R x))} in x)
    pair (R x) (R y) = R (x, y)
    fst_ (R (x, _)) = R x
    snd_ (R (_, y)) = R y
