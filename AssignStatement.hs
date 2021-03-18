module AssignStatement where
    
    import MathExpression
    import Types
    
    data AssignStatement = AssignStatement {name :: String, value :: MathExpression} deriving (Show)

    createAssignStatement :: [String] -> (AssignStatement, [String])
    createAssignStatement s = (AssignStatement (head (fst helpList)) (createMathExpression (tail (fst helpList))), (snd helpList)) where helpList = createAssignStatementHelper s

    createAssignStatementHelper :: [String] -> ([String], [String])
    createAssignStatementHelper [] = ([],[])
    createAssignStatementHelper (x:[]) = ([x], [])
    createAssignStatementHelper (x:y:z)
        | (isVar x) && (y == "=") = (x:(fst initialList), (snd initialList)) 
        | (isOp x) || (isNum x) || ((isVar x) && (y /= "=")) || (isParenth x) = (x:(fst newList), (snd newList)) 
        | otherwise = error "Assignment error"        
        where (newList, initialList) = (createAssignStatementHelper (y:z), createAssignStatementHelper z)
