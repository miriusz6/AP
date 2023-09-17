import Text.XHtml (red)
import Control.Monad

data FBTree a = Leaf a | Node [FBTree a]
  deriving (Eq, Show)

myTree1, myTree2 :: FBTree Int
myTree1 = Node [Leaf 2, Node [Leaf 3, Leaf 5], Node [Leaf 7]]
myTree2 = Node [Leaf 2, Node [Leaf 3, Leaf 5, Node []], Node [Leaf 7]]


reduce :: (a -> a -> a) -> FBTree a -> a
reduce f (Node [head]) = reduce f head
reduce f (Node tree) = f (reduce f (head tree)) (reduce f (Node (tail tree)))  
reduce f (Leaf x) = x

result = reduce (+) myTree1


reduceM :: Monad m => (a -> a -> m a) -> FBTree a -> m a
reduceM f (Node [head]) = reduceM f head
reduceM f (Node tree) =
    reduceM f (head tree) >>= \a -> reduceM f (Node (tail tree)) >>= \b -> f a b 
reduceM f (Leaf x) = return x

addMaybe :: Int -> Int -> Maybe Int
addMaybe x y = return $ x+y

resultM = reduceM addMaybe myTree1

addTrace :: Int -> Int -> IO Int
addTrace x y =
  do let s = x+y
     putStrLn $ show x ++ "+" ++ show y ++ "=" ++ show s
     return s

resultTrace = reduceM addTrace myTree1

type Count = Int


-- newtype IntState a = State {runSt :: Int -> (a, Int)}
-- 
-- instance Monad IntState where
--     return a = State (\s -> (a, s))
--     m >>= f = State (\s0 -> let (a,s1) = runSt m s0
--         in runSt (f a) s1)

--newtype IntState a = St {runSt :: Int -> (a, Int)}
--
--
instance Functor (Reader d) where
   fmap f m = m >>= \a -> return (f a)

instance Applicative (Reader d) where
   pure = return; (<*>) = ap


newtype Reader d a = Rd {runRd :: d -> a}

instance Monad (Reader d) where
   return a = Rd (\d -> a)
   m >>= f = Rd (\d -> let a=runRd m d in runRd (f a) d)

--data Expr = Const Int | Var String | Plus Expr Expr | Let String Expr Expr
--
--local :: (d -> d) -> Reader d a -> Reader d a
--local f m = Rd (\d -> runRd m (f d))
--
--ask :: Reader d d
--ask = Rd (\d -> d)
--
--eval :: Expr -> Reader (String -> Int) Int
--eval (Const n) = return n
--eval (Var x) = do r <- ask; return (r x)
--eval (Plus e1 e2) =
--    do n1 <- eval e1; n2 <- eval e2; return (n1 + n2)
--eval (Let x e1 e2) =
--     do n1 <- eval e1 local (\r -> \y -> if y == x then n1 else r y) (eval e2)

-- res = runRd (eval (Const 5)) (\x -> error $ "kupa")

-- sumCount :: FBTree Int -> (Int, Count)





newtype Writer w a = Writ { runWriter :: (a,w) }

instance Applicative (Writer w) where
    pure = return; (<*>) = ap

instance Functor (Writer d) where
    fmap f m = m >>= \a -> return (f a)



instance (Monoid w) => Monad (Writer w) where
    return a = Writ (a,mempty)
    (Writ (a,w)) >>= f = let (a',w') = runWriter $ f a in Writ (a',w `mappend` w')
