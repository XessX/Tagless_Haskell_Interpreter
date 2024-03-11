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
    leql :: repr Int -> repr Int -> repr Bool
    lth :: repr Int -> repr Int -> repr Bool
    neql :: repr Int -> repr Int -> repr Bool
    eql :: repr Int -> repr Int -> repr Bool
    gtr :: repr Int -> repr Int -> repr Bool
    gtre :: repr Int -> repr Int -> repr Bool
    if_el :: repr Bool -> repr a -> repr a -> repr a
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b
    fix :: (repr a -> repr a) -> repr a
    fst_ :: repr (a, b) -> repr a
    snd_ :: repr (a, b) -> repr b
    pair :: repr a -> repr b -> repr (a, b)