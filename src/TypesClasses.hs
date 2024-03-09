module TypesClasses where

-- Define data types if they are used in the language
data MDouble = M Double 
    | VZ
data NInt = N Int 
    | VZ2

-- Define the type class for expressions
class ExpSYM repr where
    int :: Int -> repr Int
    bool :: Bool -> repr Bool
    float :: Float -> repr Float
    add :: repr Int -> repr Int -> repr Int
    neg :: repr Int -> repr Int
    sub :: repr Int -> repr Int -> repr Int
    mul :: repr Int -> repr Int -> repr Int
    leq :: repr Int -> repr Int -> repr Bool
    lt :: repr Int -> repr Int -> repr Bool
    neq :: repr Int -> repr Int -> repr Bool
    eq :: repr Int -> repr Int -> repr Bool
    gt :: repr Int -> repr Int -> repr Bool
    gte :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a
    fst_ :: repr (a, b) -> repr a
    snd_ :: repr (a, b) -> repr b
    pair :: repr a -> repr b -> repr (a, b)