module Statement where
    
    import Variable
    import Types
    import MathExpression
    import Functions
    import System.IO.Unsafe

    data Statement =  AssignStatement {name :: String, value :: MathExpression}
                    | PrintStatement [MathExpression] 
                    | IfStatement {condition :: MathExpression, body :: [Statement], elseBody :: [Statement]}
                    | WhileStatement {condition :: MathExpression, body :: [Statement]}
                    deriving (Show)

    executeStatements :: [Statement] -> IO ()
    executeStatements l = do
                        putStr "\n"
                        executeStatementsHelper l initializeVarList

    executeStatementsHelper :: [Statement] -> [Var] -> IO ()
    executeStatementsHelper [] _ = putStr ""
    executeStatementsHelper ((AssignStatement n e):xs) v = executeStatementsHelper xs (addVar n (evaluate e v) v)
    executeStatementsHelper ((PrintStatement l):xs) v = do
                                                    evaluatePrintStatement l v
                                                    executeStatementsHelper xs v
    executeStatementsHelper ((IfStatement c b e):xs) v
        | cond = executeStatementsHelper (b ++ xs) v--evaluateIfStatement (b,xs) v
        | otherwise = executeStatementsHelper (e ++ xs) v--evaluateIfStatement (e,xs) v
        where cond = numToBool (evaluate c v)
    executeStatementsHelper ((WhileStatement c b):xs) v
        | cond = executeStatementsHelper (b ++ ((WhileStatement c b):xs)) v
        | otherwise = executeStatementsHelper xs v
        where cond = numToBool (evaluate c v)

    createStatements :: [String]->[Statement]
    createStatements [] = []
    createStatements s = (fst newStat):recursion
                  where  newStat = createStatementsHelper s
                         recursion = createStatements (snd newStat)

    createStatementsHelper :: [String]->(Statement, [String])
    createStatementsHelper [] = error "creating statements helper with empty list"
    createStatementsHelper (x:y:z)
        | (isVar x) && (y == "=") = assignStat
        | x == "print" = printStat
        | x == "while" = whileStat
        | x == "if" = ifStat
        | otherwise = error "we dont know what kind of statement this\n"
        where   assignStat = createAssignStatement (x:y:z)
                printStat = createPrintStatement (y:z)
                whileStat = createWhileStatement (y:z)
                ifStat = createIfStatement (y:z)

    -- If Statement
    createIfStatement :: [String] -> (Statement, [String])
    createIfStatement l = (IfStatement (fst parsed) (fst newList) (fst elseList), snd elseList)
        where parsed = parseAndCreateMathExpression l
              newList = groupMultiStatements (snd parsed)
              elseList = createIfStatementHelper (snd newList)

    createIfStatementHelper :: [String] -> ([Statement], [String])
    createIfStatementHelper ("else":xs) = groupMultiStatements xs
    createIfStatementHelper l = ([], l)

    -- While Statement
    createWhileStatement :: [String] -> (Statement, [String])
    createWhileStatement l = (WhileStatement (fst parsed) (fst newList), snd newList)
                    where parsed = parseAndCreateMathExpression l
                          newList = groupMultiStatements (snd parsed)

    --Print Statement
    createPrintStatement :: [String] -> (Statement, [String])
    createPrintStatement (x:[]) = (PrintStatement [(createMathExpression [x])], [])
    createPrintStatement s = (PrintStatement (fst helpList), (snd helpList)) where helpList = createPrintStatementHelper s
    
    createPrintStatementHelper :: [String] -> ([MathExpression], [String])
    createPrintStatementHelper l
        | (snd parsed) /= [] && head (snd parsed) == "," = ((fst parsed):(fst newList), snd newList)
        | otherwise = ([(fst parsed)], snd parsed)
        where parsed = parseAndCreateMathExpression l
              newList = createPrintStatementHelper (tail (snd parsed))

    evaluatePrintStatement :: [MathExpression] -> [Var] -> IO ()
    evaluatePrintStatement [] _ = putStr "\n"
    evaluatePrintStatement (x:xs) v = do 
                                    putStr ((show (evaluate x v)) ++ " ")
                                    evaluatePrintStatement xs v

    -- Assign Statement
    createAssignStatement :: [String] -> (Statement, [String])
    createAssignStatement (x:y:s)
        | (isVar x && y == "=") = (AssignStatement x (fst helpList), (snd helpList)) 
        | otherwise = error "not an assignment statement"
        where helpList = parseAndCreateMathExpression s
    
    -- Grouping Multi-Statement, also just takes the next statement
    groupMultiStatements :: [String]->([Statement], [String])
    groupMultiStatements [] = ([],[])
    groupMultiStatements ("{":xs) = (createStatements (fst newList), snd newList) where newList = (groupMultiStatementsHelp xs 1)
    groupMultiStatements l = ([fst newList], snd newList) where newList = createStatementsHelper l

    groupMultiStatementsHelp :: [String] -> Integer -> ([String], [String])
    groupMultiStatementsHelp ("}":xs) 1 = ([], xs)
    groupMultiStatementsHelp ("}":xs) n = ("}":(fst newList),snd newList) where newList = groupMultiStatementsHelp xs (n-1)
    groupMultiStatementsHelp ("{":xs) n = ("{":(fst newList),snd newList) where newList = groupMultiStatementsHelp xs (n+1)
    groupMultiStatementsHelp (x:xs) n = (x:(fst newList),snd newList) where newList = groupMultiStatementsHelp xs n

