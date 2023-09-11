type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)

moves :: [Direction] -> Pos -> Pos
moves [] initPos = initPos
moves (fstDir:remainDirs) initPos = move fstDir (moves remainDirs initPos)

data Nat = Zero | Succ Nat
    deriving (Eq, Show, Read, Ord) 

natAdd :: Nat -> Nat -> Nat
natAdd nonZero Zero = nonZero
natAdd acc (Succ iter) = natAdd (Succ acc) iter

natAddTimes :: Nat -> Nat -> Nat -> Nat
natAddTimes sum toAdd Zero = sum
natAddTimes sum Zero _ = sum
natAddTimes sum toAdd (Succ times)
    | times == Zero = (natAdd sum toAdd)
    | otherwise = natAddTimes (natAdd sum toAdd) toAdd times

natMult :: Nat -> Nat -> Nat
natMult _ Zero = Zero
-- natMult Zero _ = Zero
-- natMult acc (Succ iter) = natAddTimes acc acc iter
natMult acc (Succ iter) = natAdd acc (natMult acc iter)   


natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ foo) = 1 + (natToInt foo)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat nonZero = Succ (intToNat (nonZero-1)) 



zero = Zero
one = Succ Zero
two = Succ one
three = Succ two


data Tree = Leaf | Node Int Tree Tree
    deriving (Eq, Show, Read, Ord)

treeInsert :: Int -> Tree -> Tree
treeInsert insVal Leaf = Node insVal Leaf Leaf
treeInsert insVal tree@(Node nodeVal treeL treeR)
    | insVal == nodeVal = tree
    | insVal < nodeVal = Node nodeVal (treeInsert insVal treeL) treeR 
    | insVal > nodeVal = Node nodeVal treeL (treeInsert insVal treeR)



threeN = Node 10 Leaf Leaf
oneN = Node 1 Leaf Leaf
twoN = Node 5 oneN threeN


data PTree a = PLeaf | PNode a (PTree a) (PTree a)
    deriving (Eq, Show, Read, Ord)

pTreeInsert :: (Ord a) => a -> PTree a -> PTree a
pTreeInsert insVal PLeaf = PNode insVal PLeaf PLeaf
pTreeInsert insVal tree@(PNode nodeVal treeL treeR)
    | insVal == nodeVal = tree
    | insVal < nodeVal = PNode nodeVal (pTreeInsert insVal treeL) treeR 
    | insVal > nodeVal = PNode nodeVal treeL (pTreeInsert insVal treeR)


class Sizeable t where
  size :: t -> Int

instance Sizeable Int where
  -- size :: Int -> Int
  size _ = 1




-- instance Sizeable [a] where 
--     size :: [a] -> Int
--     size lst = length lst

instance Sizeable a => Sizeable [a] where
    size :: Sizeable a => [a] -> Int
    size (head : []) = size head
    size (head : tail) = size head + size tail



