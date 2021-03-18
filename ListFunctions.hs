module ListFunctions where

    import Functions
    import Types

    splitWords :: String -> [String]
    splitWords s = words (splitWord s)

    splitWord :: [Char] -> [Char]
    splitWord [] = []
    splitWord (x:[]) = [x]
    splitWord (x:y:z) = if (diffTypes x y) then [x] ++ [' '] ++ (splitWord (y:z)) else [x] ++ (splitWord (y:z))

    diffTypes :: Char->Char->Bool
    diffTypes a b = not (or [and [isAlphaNum a, isAlphaNum b], and [isOperator a, isOperator b]])

