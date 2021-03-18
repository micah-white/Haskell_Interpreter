module Types where

    import Functions

    getType :: String->String
    getType s
        | isVar s = "var"
        | isKeyword s = "kw"
        | isNum s = "num"
        | isPunctuation (head s) = "punc"
        | isOperator (head s) = "operator"
        | otherwise = error "IDK what type this is"

    isAlphaNum :: Char -> Bool
    isAlphaNum c = or [isAlpha c, isDigit c]

    isAlpha :: Char -> Bool
    isAlpha c = or [isLower c, isUpper c]

    isLower :: Char -> Bool
    isLower c = or [c == 'q', c == 'w', c == 'e', c ==  'r', c ==  't', c ==  'y', c ==  'u',  c == 'i', c ==  'o',  c == 'p', c ==  'a',  c == 's',  c == 'd',  c == 'f',  c == 'g', c ==  'h', c ==  'j',  c == 'k',  c == 'l', c ==  'z', c ==  'x', c ==  'c', c ==  'v', c ==  'b', c ==  'n', c ==  'm']

    isUpper :: Char -> Bool
    isUpper c = or [c == 'Q',c ==  'W', c == 'E',c ==  'R',c ==  'T', c == 'Y', c == 'U',c ==  'I',c ==  'O',c ==  'P',c ==  'A',c ==  'S',c ==  'D',c ==  'F',c ==  'G', c == 'H',c ==  'J',c ==  'K', c == 'L', c == 'Z', c == 'X',c ==  'C',c ==  'V', c == 'B', c == 'N',c ==  'M']

    isDigit :: Char -> Bool
    isDigit c = or [c == '0', c == '1', c == '2', c == '3', c == '4', c == '5', c == '6', c == '7', c == '8', c == '9']

    isPunctuation :: Char -> Bool
    isPunctuation c = or [c == '(',  c == ')' ,  c ==  '{',c ==  '}',  c ==  ',']

    isOperator :: Char -> Bool
    isOperator c = or [c == '+',c == '=',  c == '<',c ==  '>',c == '-',c == '>',c == '/',c == '%',c == '|',c == '&',c ==  '*',c ==  '!']

    isOp :: String -> Bool
    isOp (x:xs) = isOperator x && (x:xs) /= "="

    isKeyword :: String->Bool
    isKeyword s = member s ["print", "if", "else", "while"]

    isVar :: String->Bool
    isVar s = not(isKeyword s) && isAlpha (head s)

    isNum :: String->Bool
    isNum s = isDigit(head s)

    isParenth :: String->Bool
    isParenth (x:_) = (x == '(') || (x == ')')
