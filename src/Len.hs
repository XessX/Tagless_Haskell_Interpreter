module Len where

import TypesClasses

-- Computes Length of the program
newtype Len a = Len { unLen :: Int }
deriving instance Eq a => Eq (Len a)

instance ExpSYM Len where
    int _  = Len 1
    bool _ = Len 1
    float _ = Len 1
    lam f = Len( unLen (f (Len 0)) + 1 )
    app e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    fix f = Len( unLen (f (Len 0)) + 1 )
    add e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    sub e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    mul e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    neg e1 = Len( unLen e1 + 1 )            
    leql e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    lth e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    neql e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    eql e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    gtr e1 e2 = Len( unLen e1 + unLen e2 + 1 )
    gtre e1 e2 = Len( unLen e1 + unLen e2 + 1 )    
    if_el be et ee = Len( unLen be +  unLen et + unLen ee  + 1 )
    fst_ e = Len(unLen e + 1)
    snd_ e = Len(unLen e + 1)    
    pair e1 e2 = Len(unLen e1 + unLen e2 + 1) 