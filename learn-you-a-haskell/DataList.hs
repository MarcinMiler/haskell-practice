import Data.List
import Data.Function

intersperse' = intersperse '.' "HEY"

-- H.E.Y

intercalate' = intercalate [0] [[1,2,3],[4,5,6]]

-- [1,2,3,0,4,5,6]

transpose' = transpose [[1,2,3], [4,5,6]]

-- [[1,4], [2,5], [3,6]]

concat' = concat [[1,2], [3,4]]

-- [1,2,3,4]

concatMap' = concatMap (replicate 4) [1..3]

-- [1,1,1,1,2,2,2,2,3,3,3,3]

and' = and $ map (>4) [4,5,6]

-- True

or' = or $ map (==4) [1,2,3,4]

-- True

any' = any (==1) [1,2,3,4]

-- False

all' = all (==4) [4,4,4]

-- True

iterate' = take 3 $ iterate (*2) 1

-- [1,2,4]

splitAt' = splitAt 2 "Hey"

-- ("He", "y")

takeWhile' = takeWhile (>4) [6,5,4,3,2,1]

-- [6,5]

dropWhile' = dropWhile (<3) [1,2,3,4,5,6]

-- [4,5,6]

sort' = sort [3,1,4,2]

-- [1,2,3,4]

group' = group [1,1,2,2,3,3]

-- [[1,1],[2,2],[3,3]]

inits' = inits "hey"

-- ["", "h", "he", "hey"]

tails' = tails ["hey"]

-- ["hey", "he", "h", ""]

partition' = partition (<3) [1,2,3,4,5,6]

-- [[1,2,3], [4,5,6]]

find' = find (==1) [1,2,3]

-- Just 1

elemIndex' = 4 `elemIndex` [1,2,3,4,5]

-- Just 3

elemIndices' = elemIndices ' ' "Hi Hay Hello"

-- [2,6]

findIndex' = findIndex (==4) [1,2,3,4]

-- Just 3

lines' = lines "first\nsecond\nthird"

-- ["first", "second", "third"]

unlines' = unlines ["first", "second", "third"]

-- "first\nsecond\nthird"

words' = words "hi hy hello"

-- ["hi", "hay", "hello"]

unwords' = unwords ["hi", "hay", "hello"]

-- "hi hy hello"

nub' = nub [1,1,2,2,2,3,3,3,3]

-- [1,2,3]

delete' = delete 'h' "hey"

-- "ey"

diff' = [1..10] \\ [2,5,9]

-- [1,3,4,6,7,8,10]

union' = [1..7] `union` [1..10]

-- [1,2,3,4,5,6,7,8,9,10]

intersection' = [1..5] `intersect` [3..7]

-- [3,4,5]

insert' = insert 4 [1,2,3,5]

-- [1,2,3,4,5]

groupBy' = groupBy (\x y -> (x > 0) == (y > 0)) [-3, -2, -1, 1, 2, -5, -6]

-- [[-3, -2, -1],[1,2],[-5,-6]]

sortBy' = sortBy (compare `on` length) [[], [2,2], [2]]

-- [[], [2], [2,2]]
