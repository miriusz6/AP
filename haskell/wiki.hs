import Control.Monad  -- you will need to put this towards the top of the file





data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)


coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)
push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)


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


--coinS, pushS :: State TurnstileState TurnstileOutput
--coinS = State coin
--pushS = State push


mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  pure [a1]--, a2, a3, a4, a5]


evalState :: State s a -> s -> a
evalState p s = fst (runState p s)

execState :: State s a -> s -> s
execState p s = snd (runState p s)

put :: s -> State s ()
put newState = state $ \_ -> ((), newState)

get :: State s s
get = state $ \s -> (s, s)


-- pushS = do
--   s <- get
--   put Locked
--   case s of
--     Locked   -> return Tut
--     Unlocked -> return Open


pushS :: State TurnstileState TurnstileOutput
pushS = do
  s <- get
  put Locked
  case s of
    Locked   -> return Tut
    Unlocked -> return Open


coinS :: State TurnstileState TurnstileOutput
coinS = do
  s <- get
  --put Locked
  case s of
    Locked   -> return Open
    Unlocked -> return Thank


blahf :: State TurnstileState TurnstileOutput
blahf = return Tut
    --Unlocked -> return Open