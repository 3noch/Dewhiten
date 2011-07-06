module DewhitenCore where
    data IndentOptions = UseBlanks | CopyIndent deriving (Show,Eq)
    
    dewhitenString :: IndentOptions -> String -> String
    dewhitenString indent = unlines . (dewhitenLines indent) . lines
    
    dewhitenLines :: IndentOptions -> [String] -> [String]
    dewhitenLines UseBlanks  = (map trimLine) . trimList
    dewhitenLines CopyIndent = indentLines . (map trimNonWhiteLine) . trimList
    
    indentLines :: [String] -> [String]
    indentLines []  = []
    indentLines [x] = [trimLine x]
    indentLines xs  = indentLines newList ++ [ultimate]
                      where newList     = take lenLess2 xs ++ [indentPair penultimate ultimate]
                            lenLess2    = length xs - 2
                            penultimate = last (init xs)
                            ultimate    = last xs
    
    indentPair :: String -> String -> String
    indentPair a b = if isWhiteLine a then takeWhile isWhite b else a
    
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
