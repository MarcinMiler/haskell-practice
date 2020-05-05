import qualified Data.Foldable as F

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Show, Read, Eq)

instance Functor BinaryTree where
    fmap f Leaf = Leaf
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance F.Foldable BinaryTree where
    foldMap f Leaf = mempty
    foldMap f (Node left a right) = 
        F.foldMap f left `mappend` f a `mappend` F.foldMap f right

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left a right)
    | x == a = Node left a right
    | x > a  = Node left a (insert' x right)
    | x < a  = Node (insert' x left) a right
