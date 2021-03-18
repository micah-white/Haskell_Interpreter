module MathExpression where 

    import Functions
    import Variable
    import Types

    -- MATH STATEMENT
    data MathExpression = Num Integer | Expression {operator :: String, left :: MathExpression, right :: MathExpression} | Variable String deriving (Show)

    evaluate :: MathExpression -> [Var] -> Integer
    evaluate (Num x) _ = x
    evaluate (Variable s) varList = getVar s varList
    evaluate (Expression "*" x y) varList = (evaluate x varList) * (evaluate y varList)
    evaluate (Expression "/" x y) varList = div (evaluate x varList) (evaluate y varList)
    evaluate (Expression "+" x y) varList = (evaluate x varList) + (evaluate y varList)
    evaluate (Expression "-" x y) varList = (evaluate x varList) - (evaluate y varList)
    evaluate (Expression "||" x y) varList = boolToNum((numToBool(evaluate x varList)) || (numToBool(evaluate y varList)))
    evaluate (Expression "&&" x y) varList = boolToNum((numToBool(evaluate x varList)) && (numToBool(evaluate y varList)))
    evaluate (Expression "<" x y) varList = boolToNum(evaluate x varList < evaluate y varList)
    evaluate (Expression "<=" x y) varList = boolToNum(evaluate x varList <= evaluate y varList)
    evaluate (Expression ">" x y) varList = boolToNum(evaluate x varList > evaluate y varList)
    evaluate (Expression ">=" x y) varList = boolToNum(evaluate x varList >= evaluate y varList)
    evaluate (Expression "==" x y) varList = boolToNum((evaluate x varList) == (evaluate y varList))
    evaluate (Expression "!=" x y) varList = boolToNum((evaluate x varList) /= (evaluate y varList))
    evaluate (Expression "%" x y) varList = rem (evaluate x varList) (evaluate y varList)

    createMathExpression :: [String] -> MathExpression
    createMathExpression [] = error "creating math expression from empty list"
    createMathExpression (s:[])
        | isNum s = Num (read s :: Integer)
        | otherwise = Variable s
    createMathExpression (x:y:z) = createMathExpressionHelp (groupify (listify (x:y:z)))

    createMathExpressionHelp :: [[String]] -> MathExpression
    createMathExpressionHelp [] = error "creating math expression help from empty list"
    createMathExpressionHelp(s:[]) = createMathExpression s
    createMathExpressionHelp l = Expression (head op) (createMathExpressionHelp (fst newList)) (createMathExpressionHelp (snd newList))
                                 where  op = argminLast pemdas l
                                        newList = splitBy op l

    groupify :: [[String]]->[[String]]
    groupify [] = []
    groupify ((x:[]):[]) = ((x:[]):[])
    groupify ((x:xs):[]) = ((x:xs):[])
    groupify (["("]:xs) = (head newList):(groupify (tail newList)) where newList = (groupifyHelp xs 1)
    groupify (x:xs) = x:(groupify xs)

    groupifyHelp :: [[String]] -> Integer -> [[String]]
    groupifyHelp ([")"]:xs) 1 = []:xs
    groupifyHelp ([")"]:xs) n = ([")"]++(head newList)):(tail newList) where newList = groupifyHelp xs (n-1)
    groupifyHelp (["("]:xs) n = (["("]++(head newList)):(tail newList) where newList = groupifyHelp xs (n+1)
    groupifyHelp (x:xs) n = (x ++ (head newList)):(tail newList) where newList = groupifyHelp xs n

    pemdas :: [String] -> Integer
    pemdas (x:[])
        | x == "||" = 0
        | x == "&&" = 1
        | member x ["!=", "=="] = 2
        | member x [">=", "<=", "<", ">"] = 3
        | member x ["+", "-"] = 4
        | member x ["*", "/", "%"] = 5
        | isNum x = 7
    pemdas (x:xs) = 6

    parseMathExpression :: [String] -> ([String], [String])
    parseMathExpression [] = ([],[])
    parseMathExpression (x:[]) = ([x], [])
    parseMathExpression (x:y:z)
        | (isOp x) || (isNum x) || ((isVar x) && (y /= "=")) || (isParenth x) = (x:(fst newList), (snd newList)) 
        | otherwise = ([], (x:y:z))       
        where (newList, initialList) = (parseMathExpression (y:z), parseMathExpression z)


    parseAndCreateMathExpression :: [String] -> (MathExpression, [String])
    parseAndCreateMathExpression l = (createMathExpression (fst newList), snd newList) where newList = parseMathExpression l
