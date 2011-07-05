module DewhitenCore where
    data IndentOptions = UseBlanks | CopyIndent
    
    dewhitenString :: String -> String
    dewhitenString = unlines . (dewhitenLines CopyIndent) . lines
    
    dewhitenLines :: IndentOptions -> [String] -> [String]
    dewhitenLines UseBlanks  = (map trimLine) . trimList
    dewhitenLines CopyIndent = indentLines . (map trimNonWhiteLine) . trimList
    
    indentLines :: [String] -> [String]
    indentLines []  = []
    indentLines [x] = [trimLine x]
    indentLines xs  = indentLines (init newList)
                      where newList = take lenLess2 xs ++ indentPair (penultimate xs) (last xs)
                            lenLess2 = length xs - 2
                            penultimate = last . init
    
    indentPair :: String -> String -> [String]
    indentPair a b = [if isWhiteLine a then takeWhile isWhite b else a,
                      if isWhiteLine b then b else b]
    
    trimNonWhiteLine :: String -> String
    trimNonWhiteLine a
        | isWhiteLine a = a
        | otherwise     = trimLine a
    
    trimLine :: String -> String
    trimLine = reverse . dropWhile isWhite . reverse
    
    trimList :: [String] -> [String]
    trimList = reverse . dropWhile isWhiteLine . reverse
    
    isWhiteLine :: String -> Bool
    isWhiteLine = all isWhite
    
    isWhite :: Char -> Bool
    isWhite ' '  = True
    isWhite '\t' = True
    isWhite '\n' = True
    isWhite '\r' = True
    isWhite _    = False
