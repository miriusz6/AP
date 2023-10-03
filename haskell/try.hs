doubleMe x = x + x 

doubleUs x y = doubleMe x + doubleMe y  

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 


data Vector = Vector a a a deriving (Show)  

data VectorChar a = VectorChar a Int Int deriving (Show)  

