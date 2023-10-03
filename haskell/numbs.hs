import Control.Monad  -- you will need to put this towards the top of the file





-- data TurnstileState = Locked | Unlocked
--   deriving (Eq, Show)

-- data TurnstileOutput = Thank | Open | Tut
--   deriving (Eq, Show)


-- coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
-- coin _ = (Thank, Unlocked)
-- push Locked   = (Tut , Locked)
-- push Unlocked = (Open, Locked)


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



addWLog:: Int -> Int -> State String Int 
addWLog x y = do
  s <- get
  put (s ++ "Added " ++ show x ++ " and " ++ show y ++ " -> ")
  return (x + y)


applyWLog :: Show a => Show b => Show c => (a -> b -> c) -> a -> b -> State String c
applyWLog (+) x y  = do
  let z = x + y
  s <- get
  put ("Added " ++ show x ++ " and " ++ show y ++ " got " ++ show z ++ " | ")
  return z
-- applyWLog (-) x y  = do
--   let z = x + y
--   s <- get
--   put ("Subtracted " ++ show x ++ " and " ++ show y ++ " got " ++ show z ++ " | ")
--   return z


-- log_ = evalState (addWLog 5 10) 5
-- val_ = execState (addWLog 5 10) 5




--reduceM f (head tree) >>= \a -> reduceM f (Node (tail tree)) >>= \b -> f a b 
--reduceM f (Leaf x) = return x

twiceAddWLog:: Int -> Int -> State String Int
twiceAddWLog x y = do
  a <- addWLog 5 10
  b <- addWLog 20 10
  return (a + b)