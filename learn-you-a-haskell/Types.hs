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




