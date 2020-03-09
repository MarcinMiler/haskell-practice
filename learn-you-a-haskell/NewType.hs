import Control.Applicative

zipList = getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]

-- [2,200,15]

-- data ZipList a = ZipList [a]  

-- data ZipList a = ZipList { getZipList :: [a] }

-- newtype ZipList a = ZipList { getZipList :: [a] }

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)

pair = getPair $ fmap (*100) (Pair (2,3)) 

-- Lazines

-- data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"

-- helloMe undefined -- exception

newtype CoolBool = CoolBool { getCoolBool :: Bool }

coolBool = helloMe undefined

-- type vs. newtype vs. data

-- for type aliases
type IntList = [Int]

-- for wraping and unwraping
newtype CharList = CharList { getCharList :: [Char] }

-- for multiple constructors
data CoolData = Cool | NotColl