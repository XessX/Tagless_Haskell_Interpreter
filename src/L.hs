module L where

import TypesClasses

-- Computes Length of the program
newtype L a = L { unL :: Int }
deriving instance Eq a => Eq (L a)

instance ExpSYM L where
    int _ = L 1
    bool _ = L 1
    float _ = L 1
    add (L x) (L y) = L (x + y + 1)
    neg (L x) = L (x + 1)
    sub (L x) (L y) = L (x + y + 1)
    mul (L x) (L y) = L (x + y + 1)
    leq (L x) (L y) = L (x + y + 1)
    lt (L x) (L y) = L (x + y + 1)
    neq (L x) (L y) = L (x + y + 1)
    eq (L x) (L y) = L (x + y + 1)
    gt (L x) (L y) = L (x + y + 1)
    gte (L x) (L y) = L (x + y + 1)
    if_ (L b) (L x) (L y) = L (b + x + y + 1)
    lam f = L (unL (f (L 1)) + 1)
    app (L f) (L x) = L (f + x + 1)
    fix f = L (unL (f (L 1)) + 1)
    fst_ (L x) = L (x + 1)
    snd_ (L y) = L (y + 1)
    pair (L x) (L y) = L (x + y + 1)