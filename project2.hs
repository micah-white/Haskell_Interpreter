
import System.IO
import System.IO.Unsafe


interpret :: String -> IO ()
interpret filename = executeStatements (createStatements (splitWords (fileToString filename)))




-- Statement

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
    where newStat = createStatementsHelper s
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


-- Variable
data Var = Var {varID :: String, val :: Integer} deriving (Show, Eq)
    
initializeVarList :: [Var]
initializeVarList = []

addVar :: String->Integer->[Var]->[Var]
addVar name value varList = (Var name value):(removeVar name varList)

removeVar :: String->[Var]->[Var]
removeVar _ [] = []
removeVar s (v:vs) = if (s == (varID v)) then vs else v:(removeVar s vs)

getVar :: String->[Var]->Integer
getVar _ [] = error "Variable does not exist"
getVar s (v:vs) = if (s == (varID v)) then (val v) else getVar s vs

-- ListFunctions
splitWords :: String -> [String]
splitWords s = words (splitWord s)

splitWord :: [Char] -> [Char]
splitWord [] = []
splitWord (x:[]) = [x]
splitWord (x:y:z) = if (diffTypes x y) then [x] ++ [' '] ++ (splitWord (y:z)) else [x] ++ (splitWord (y:z))

diffTypes :: Char->Char->Bool
diffTypes a b = not (or [and [isAlphaNum a, isAlphaNum b], and [isOperator a, isOperator b]])



-- Types

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


-- FileIO

copyfile infile outfile =
    do
        inh <- openFile infile ReadMode
        outh <- openFile outfile WriteMode
        copy inh outh
        hClose inh
        hClose outh
    

copy inh outh =
    do
        atEof <- hIsEOF inh
        if atEof
            then return ()
            else do 
                c <- hGetChar inh
                hPutChar outh c
                copy inh outh

fileToString :: String -> String
fileToString infile =
    do
        text <- unsafePerformIO (readFile infile)
        return text

--Functions

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

rev list = if null list then [ ]
    else rev (tail list) ++ [head list]

inRange low high list = [x | x<-list, x>=low, x<= high]

zipWith f xs ys = map (uncurry f) (zip xs ys)

qsort [ ] = [ ]
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
                smaller = [y | y<-xs, y<x]
                larger = [y | y<-xs, y>= x]

member :: (Eq a) => a->[a]->Bool
member _ [] = False
member x (h:t) = (x==h) || (member x t)

numToBool :: Integer->Bool
numToBool 0 = False
numToBool _ = True

boolToNum :: Bool->Integer
boolToNum False = 0
boolToNum True = 1

listify :: [a] -> [[a]]
listify [] = []
listify (x:xs) = [x]:(listify xs)

removeLast :: [a]->[a]
removeLast (x:[]) = []
removeLast (x:xs) = x:(removeLast xs)

findFirst :: (a->Bool) -> [a] -> a
findFirst _ [] = error "no element meets condition\n"
findFirst f (x:xs)
    | (f x) = x
    | otherwise = findFirst f xs

argmax :: (a -> Integer)->[a]->a
argmax _ [] = error "it's empty bruh"
argmax f (x:xs) = findFirst (\c -> ((f c) == (maximum (map f (x:xs))))) (x:xs)

argmin :: (a -> Integer)->[a]->a
argmin _ [] = error "it's empty bruh"
argmin f (x:xs) = findFirst (\c -> ((f c) == (minimum (map f (x:xs))))) (x:xs)

argmaxLast :: (a -> Integer)->[a]->a
argmaxLast _ [] = error "it's empty bruh"
argmaxLast f l = argmax f (rev l)

argminLast :: (a -> Integer)->[a]->a
argminLast _ [] = error "it's empty bruh"
argminLast f l = argmin f (rev l)

splitBy :: (Eq a)=>a->[a]->([a], [a])
splitBy _ [] = ([],[])
splitBy p (x:xs)
    | p == x = ([], xs)
    | otherwise = (x:(fst newList), snd newList)
    where newList = splitBy p xs


