-- import Text.XHtml (red)
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




newtype State s a = State { runState :: s -> (a, s) }
state :: (s -> (a, s)) -> State s a
state = State


instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = Control.Monad.return
  (<*>) = ap

instance Monad (State s) where
    return :: a -> State s a
    return x = State ( \ s -> (x, s) )
    (>>=) :: State s a -> (a -> State s b) -> State s b
    p >>= k = state $ \ s0 ->
        let (x, s1) = runState p s0  -- Running the first processor on s0.
        in runState (k x) s1         -- Running the second processor on s1.


evalState :: State s a -> s -> a
evalState p s = fst (runState p s)

execState :: State s a -> s -> s
execState p s = snd (runState p s)

put :: s -> State s ()
put newState = state $ \_ -> ((), newState)

get :: State s s
get = state $ \s -> (s, s)



reduceM :: Monad m => (a -> a -> m a) -> FBTree a -> m a
reduceM f (Node [head]) = reduceM f head
-- reduceM f (Node tree) =
--     reduceM f (head tree) >>= \a -> reduceM f (Node (tail tree)) >>= \b -> f a b 
reduceM f (Node tree) = 
    do  a <- reduceM f (head tree)
        b <- reduceM f (Node (tail tree))
        f a b
    -- same result as do !
    -- reduceM f (head tree) >>= \a -> reduceM f (Node (tail tree)) >>= \b -> f a b 
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

addWLog :: Int -> Int -> State Int Int
addWLog x y = do
  s <- get
  put (s + 1)
  return (x + y)

addWSLog :: Int -> Int -> State String Int
addWSLog x y = do
  s <- get
  put (s ++ "Added " ++ show x ++ " and " ++ show y ++ " -> ")
  return (x + y)


resultNew = reduceM addWLog myTree1

resultNewS = reduceM addWSLog myTree1

sumCount :: FBTree Int -> (Int, Count)
sumCount t = runState (reduceM addWLog t) 0



reduceMF :: MonadFail m => (a -> a -> m a) -> FBTree a -> m a
reduceMF f (Node []) = fail ""
reduceMF f (Node [head]) = reduceM f head
-- reduceM f (Node tree) =
--     reduceM f (head tree) >>= \a -> reduceM f (Node (tail tree)) >>= \b -> f a b 
reduceMF f (Node tree) = 
    do  a <- reduceM f (head tree)
        b <- reduceM f (Node (tail tree))
        f a b
    -- same result as do !
    -- reduceM f (head tree) >>= \a -> reduceM f (Node (tail tree)) >>= \b -> f a b 
reduceMF f (Leaf x) = return x

resultMF = reduceMF addMaybe myTree1

resultMFbad = reduceMF addMaybe myTree2
























newtype IntState a = IntSt {runSt :: Int -> (a, Int)}

instance Functor IntState where
    fmap f m = m >>= \a -> return (f a)

instance Applicative IntState where
    pure :: a -> IntState a
    pure a = IntSt (\s -> (a, s))
    (<*>) = ap

instance Monad IntState where
    -- return a = St (\s -> (a, s))
    m >>= f = IntSt (\s0 -> let (a,s1) = runSt m s0
        in runSt (f a) s1)


getIntState:: IntState Int
getIntState = IntSt (\i -> (i, i))





-- WORKING DEF 

-- newtype Writ s a = Writ {runWrit :: s -> (a, s)}

-- instance Functor (Writ s) where
--     fmap f m = m >>= \a -> return (f a)

-- instance Applicative (Writ s) where
--     pure :: a -> Writ s a
--     pure a = Writ (\s -> (a, s))
--     (<*>) = ap

-- instance Monad (Writ s) where
--     -- return a = St (\s -> (a, s))
--     m >>= f = Writ (\s0 -> let (a,s1) = runWrit m s0
--         in runWrit (f a) s1)



