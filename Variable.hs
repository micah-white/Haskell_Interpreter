module Variable where

    -- import Functions

    data Var = Var {name :: String, value :: Integer} deriving (Show, Eq)
    
    initializeVarList :: [Var]
    initializeVarList = []

    addVar :: String->Integer->[Var]->[Var]
    addVar name value varList = (Var name value):(removeVar name varList)

    removeVar :: String->[Var]->[Var]
    removeVar _ [] = []
    removeVar s (v:vs) = if (s == (name v)) then vs else v:(removeVar s vs)

    getVar :: String->[Var]->Integer
    getVar _ [] = error "Variable does not exist"
    getVar s (v:vs) = if (s == (name v)) then (value v) else getVar s vs
