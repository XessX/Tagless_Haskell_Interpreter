module Runs where

import TypesClasses

-- An Runs interpreter that executes expressions
newtype Runs a = Runs { unRuns :: a } 
deriving instance Eq a => Eq (Runs a)

instance ExpSYM Runs where
    int x = Runs x
    bool b = Runs b
    float f = Runs f
    add (Runs x) (Runs y) = Runs (x + y)
    neg (Runs x) = Runs (negate x)
    sub (Runs x) (Runs y) = Runs (x - y)
    mul (Runs x) (Runs y) = Runs (x * y)
    leq (Runs x) (Runs y) = Runs (x <= y)
    lt (Runs x) (Runs y) = Runs (x < y)
    neq (Runs x) (Runs y) = Runs (x /= y)
    eq (Runs x) (Runs y) = Runs (x == y)
    gt (Runs x) (Runs y) = Runs (x > y)
    gte (Runs x) (Runs y) = Runs (x >= y)
    if_ (Runs b) (Runs x) (Runs y) = Runs (if b then x else y)
    lam f = Runs (\x -> unRuns (f (Runs x)))
    app (Runs f) (Runs x) = Runs (f x)
    fix f = Runs (let {x = unRuns (f (Runs x))} in x)
    pair (Runs x) (Runs y) = Runs (x, y)
    fst_ (Runs (x, _)) = Runs x
    snd_ (Runs (_, y)) = Runs y
