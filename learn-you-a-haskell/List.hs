-- List

numbers = [1,2,3,4]

addition = [1,2] ++ [3,4]

-- [1,2,3,4]

append = 3:[1,2]

-- [1,2,3]

-- List comparing

--  [3,2,1] > [2,1,0]
--  [3,1]   == [3, 1]

h = head [1,2,3,4]

-- 1

t = tail [1,2,3]

-- [2,3]

l = last [1,2,3]

-- 3

s = sum [1,2,3]

-- 6

p = product [6,2,1,2]

-- 24

-- List ranges

r = [1..20]

r1 = take 10 (cycle [1,2,3])

-- [1,2,3,1,2,3,1,2,3,1]

t1 = 
    take 5 (repeat 5)

-- [5,5,5,5,5]

-- List comprehension

set1 = [x | x <- [1..10], x `mod` 2 == 0]

-- [2,4,6,8]

set2 = [x + y | x <- [1 .. 2], y <- [1 .. 2]]

-- [2,3,3,4]

length' xs = sum [1 | _ <- xs]

-- length' [1,2,3] -> 3

removeNonUppercase list = [x | x <- list, x `elem` ['A' .. 'Z']]

-- removeNonUppercase "AbCdE" -> 'ACE'

-- Tuples

fst' = fst (1,2)

-- 1

snd' = snd(1,2)

-- 2

zip' = zip [1..5] ["1", "2", "3", "4", "5"]

-- [(1,"1"),(2,"2"),(3,"3"),(4,"4"),(5,"5")]



