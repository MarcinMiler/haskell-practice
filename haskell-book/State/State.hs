import System.Random

newtype State s a =
    State { runState :: s -> (a, s) }

data Die =
      DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Show)

intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        _ -> error $ "err"

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, s3) = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)

