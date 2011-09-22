module DewhitenCore where
    type Lines    = [String]
    type Modifier = Lines -> Lines

    -- Functions that modify a list of lines
    addBlankIndent  = indentLines . (map trimNonWhiteLine) :: Modifier
    noBlankIndent   = map trimLine                         :: Modifier
    addTrailingLine = (++ [""]) . trimList                 :: Modifier
    noTrailingLine  = trimList                             :: Modifier

    -- Simple wrapper for applying dewhiten to a string instead of a list of lines
    dewhitenString :: Modifier -> Modifier -> String -> String
    dewhitenString lineModifier fileModifier = unlines . lineModifier . fileModifier . lines

    indentLines :: Lines -> Lines
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

    trimList = reverse . dropWhile isWhiteLine . reverse

    isWhiteLine :: String -> Bool
    isWhiteLine = all isWhite

    isWhite :: Char -> Bool
    isWhite ' '  = True
    isWhite '\t' = True
    isWhite '\n' = True
    isWhite '\r' = True
    isWhite _    = False
