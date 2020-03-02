-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 x2 y1 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- surfaceCircle = surface $ Circle 1 1 10

-- -- 314.159...

-- surfaceRectangle = surface $ Rectangle 0 0 100 100

-- -- 10000

------------------------------------------------------------------------

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float
    | Rectangle Point Point 
    deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

surfaceCircle = surface (Circle (Point 1 1) 10)

-- 314.159...

surfaceRectangle = surface (Rectangle (Point 0 0) (Point 100 100))

-- 10000

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+b) (y2+b))

nudgeCircle = nudge (Circle (Point 34 34) 10) 5 10

-- Circle (Point 39.0 44.0) 10.0

---------------------------------------------------------------------------------

-- Record syntax

-- data Person = Person { firstName :: String 
--                      , lastName :: String
--                      , age :: Int 
--                      } deriving (Show)

-- person' = Person { firstName="Marcin", lastName="Miler", age=20 }

---------------------------------------------------------------------------------

-- Derived instances

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int 
                     } deriving (Eq, Show, Read)

person1 = Person { firstName="Jacek", lastName="Kowalski", age=40 }

eqCheck = person1 == person1

-- True

readPerson = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

-- Person {firstName = "Michael", lastName = "Diamond", age = 43}

data Bool' = False' | True' deriving (Eq, Ord)

compare' = True' `compare` False'

-- GT

compare1 = True' > False'

-- True

compare2 = True' < False'

-- False

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

compare3 = Saturday > Friday

-- True

compare4 = Monday == Monday

-- True

minBound' = minBound :: Day

-- Monday

maxBound' = maxBound :: Day

-- Sunday

succ' = succ Monday

-- Tuesday

pred' = pred Saturday

-- Friday

-------------------------------------------------------------------------------

-- Type synonyms

type String' = [Char]

type AssocList k v = [(k,v)] 

--------------------------------------------------------------------------------

-- Recursive data structures

data List a = Empty | Cons a (List a) deriving (Eq, Ord, Show, Read)

list1 = 5 `Cons` Empty

-- Cons 5 Empty

list2 = 4 `Cons` (5 `Cons` Empty)

-- Cons 4 (Cons 5 Empty)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

nums = [8,6,4,1,7,3,5]

sampleTree = foldr treeInsert EmptyTree nums

contains' = 8 `treeElem` sampleTree

-- True
