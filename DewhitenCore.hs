module DewhitenCore where
    dewhitenString :: String -> String
    dewhitenString = unlines . dewhitenLines . lines
    
    dewhitenLines :: [String] -> [String]
    dewhitenLines []     = []
    dewhitenLines [x]    = [dewhitenLine (x, "")]
    dewhitenLines (x:xs) = dewhitenLine (x, head xs) : dewhitenLines xs
    
    dewhitenLine :: (String, String) -> String
    dewhitenLine linePair
        | all isWhite (fst linePair) = takeWhile isWhite (snd linePair)
        | otherwise = trimLine (fst linePair)
    
    trimLine :: String -> String
    trimLine = reverse . dropWhile isWhite . reverse
    
    trimFile :: [String] -> [String]
    trimFile = reverse . dropWhile null . reverse
    
    isWhite :: Char -> Bool
    isWhite ' '  = True
    isWhite '\t' = True
    isWhite '\n' = True
    isWhite '\r' = True
    isWhite _    = False
