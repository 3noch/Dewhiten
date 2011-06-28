module DewhitenCore where
    fixLines :: [String] -> [String]
    fixLines [] = []
    fixLines x  = fixLines (init x) ++ [fixLine (penultimate x "") (last x)]
    
    fixLine :: String -> String -> String
    fixLine penultLine ultLine
        | all isWhite ultLine = takeWhile isWhite (penultLine)
        | otherwise = trimLine ultLine
    
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
    
    penultimate :: [a] -> a -> a
    penultimate x error = if length x > 1
                          then (last . init) x
                          else error
