module L where

import TypesClasses

-- Computes Length of the program
newtype L a = L { unL :: Int }
deriving instance Eq a => Eq (L a)

instance ExpSYM L where
    int _  = L 1
    bool _ = L 1
    float _ = L 1
    lam f = L( unL (f (L 0)) + 1 )
    app e1 e2 = L( unL e1 + unL e2 + 1 )
    fix f = L( unL (f (L 0)) + 1 )
    add e1 e2 = L( unL e1 + unL e2 + 1 )
    sub e1 e2 = L( unL e1 + unL e2 + 1 )
    mul e1 e2 = L( unL e1 + unL e2 + 1 )
    neg e1 = L( unL e1 + 1 )            
    leq e1 e2 = L( unL e1 + unL e2 + 1 )
    lt e1 e2 = L( unL e1 + unL e2 + 1 )
    neq e1 e2 = L( unL e1 + unL e2 + 1 )
    eq e1 e2 = L( unL e1 + unL e2 + 1 )
    gt e1 e2 = L( unL e1 + unL e2 + 1 )
    gte e1 e2 = L( unL e1 + unL e2 + 1 )    
    if_ be et ee = L( unL be +  unL et + unL ee  + 1 )
    fst_ e = L(unL e + 1)
    snd_ e = L(unL e + 1)    
    pair e1 e2 = L(unL e1 + unL e2 + 1) 