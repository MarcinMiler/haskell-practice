import Control.Applicative

validateLength :: Int -> String -> Maybe String
validateLength maxLen s
    | (length s) > maxLen = Nothing
    | otherwise           = Just s

newtype Name = Name String
    deriving (Show, Eq, Ord)

newtype Address =
    Address String deriving (Show, Eq, Ord)

data Person = 
    Person Name Address deriving (Show, Eq)

mkName :: String -> Maybe Name
mkName name = fmap Name $ validateLength 20 name

mkAddress :: String -> Maybe Address
mkAddress address = fmap Address $ validateLength 20 address

mkPerson :: String -> String -> Maybe Person
mkPerson name address =
    Person <$> mkName name <*> mkAddress address

data Cow = Cow {
      name   :: String
    , age    :: Int
    , wieght :: Int
} deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0    = Just n
             | otherwise = Nothing

mkCow :: String -> Int -> Int -> Maybe Cow
mkCow name age weight =
    Cow <$> noEmpty name
        <*> noNegative age
        <*> noNegative weight

mkCow2 :: String -> Int -> Int -> Maybe Cow
mkCow2 name age weight =
    liftA3 Cow (noEmpty name)
               (noNegative age)
               (noNegative weight)
