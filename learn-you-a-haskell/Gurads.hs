checkTemp :: (RealFloat a) => a -> String
checkTemp temp
    | temp <= 36.6 = "Below"
    | temp <= 37.0 = "Normal"
    | temp <= 39.0 = "High"
    | otherwise = "Unknown status"
