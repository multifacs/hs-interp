{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lambda where

import Parser ( Result(..), Term(..), myparse, prettyprint )
import Data.Char ()
import Data.List ( (\\) )

data Reduction = Reduction { reductionTerm :: Term
                           , reductionType :: String
                           } deriving(Show, Eq)

--------------------------------Main methods------------------------------------

-- reduce term
-- applies consecutive beta and eta reductions to the given lambda term.
reduce :: Term -> Result
reduce term = helperReduce term False

-- bReduce term
-- applies consecutive beta reductions to the given lambda term.
bReduce :: Term -> Result
bReduce term = helperReduce term True

-- helperReduce term notEtaFlag
-- applies consecutive reductions to the given lambda term.
-- notEtaFlag with a value of True means that we don't apply eta reductions.
helperReduce :: Term -> Bool -> Result
helperReduce term notEtaFlag
    | term /= redTerm = Result (finalTerm result)
                               (1 + reductionCount result)
                               (redTerm:reductionTerms result)
                               (redType:reductionTypes result)
    | otherwise = if redType == "" then Result term 0 [] []
                  else Result term 1 [term] [redType ++ " - infinite loop"]
    where reduction = visit term notEtaFlag
          redTerm = reductionTerm reduction
          redType = reductionType reduction
          result = helperReduce redTerm notEtaFlag

-- print final result only
run :: String -> IO ()
run s = putStrLn $ (prettyprint.finalTerm.bReduce.myparse) s

-- get a list of the intermediate reductions and terms
runWReductions :: String -> [(String, String)]
runWReductions s = (\reds -> zip (reductionTypes reds) (map prettyprint $ reductionTerms reds)) (reduce.myparse $ s)

-- format and print all reductions
runp :: String -> IO ()
runp s = putStrLn $ foldl1 (++) $ map (\(x,y) -> x ++ "\t->\t" ++ y ++ "\n") $ runWReductions s

--------------------------------Helper Methods----------------------------------

-- isFreeIn vn t
-- returns true iff the var named vn is a free var in the term t.
isFreeIn :: String -> Term -> Bool
isFreeIn vn (Application x y) = isFreeIn vn x || isFreeIn vn y

isFreeIn vn (Var x) = x == vn

isFreeIn vn (Abstraction x y) = x /= vn && isFreeIn vn y

-- notFreeIn t1 t2
-- find a variable that is not free in term1 and term2.
notFreeIn :: Term -> Term -> String
notFreeIn term1 term2 = [head ((['a'..'z'] ++ ['A'..'Z']) \\ (allFreeIn term1 ++ allFreeIn term2))]

-- allFreeIn t
-- find all free variables in an expression.
allFreeIn :: Term -> String
allFreeIn (Application x y) = allFreeIn x ++ allFreeIn y

allFreeIn (Var v) = v

allFreeIn (Abstraction x y) = allFreeIn y \\ x

-- replace t var rep
-- replaces in the term t all instances of the variable named var with the term rep.
replaceVar :: Term -> String -> Term -> Term
replaceVar (Application x y) var rep = Application (replaceVar x var rep) (replaceVar y var rep)

replaceVar (Var v) var rep = if v == var then rep else Var v

-- case 1: if the abstraction binds the var we are replacing, don't replace.
-- case 2: if the abstraction's var is free in the replacement term and the var
--         we are replacing is free in the body of the abstraction, rename the
--         abstraction's var so as not be free in the union of free variables of
--         the replacement term and the abstraction's body.
-- case 3: replace normally.
replaceVar (Abstraction x y) var rep
    | x == var = Abstraction x y
    | isFreeIn x rep && isFreeIn var y = let newvar = notFreeIn y rep in
      Abstraction newvar (replaceVar (replaceVar y x (Var newvar)) var rep)
    | otherwise = Abstraction x (replaceVar y var rep)

-- visit term notEtaFlag
-- visits all terms depth-first until we can apply a beta or eta reduction.
-- notEtaFlag with a value of True means that we don't apply eta reductions.
visit :: Term -> Bool -> Reduction
visit (Application (Abstraction x y) z) _ = Reduction (replaceVar y x z) "beta"

visit (Application x y) notEtaFlag
    | xReductionTerm /= x = Reduction (Application xReductionTerm y) xReductionType
    | otherwise = Reduction (Application x yReductionTerm) yReductionType
    where xReduction = visit x notEtaFlag
          xReductionTerm = reductionTerm xReduction
          xReductionType = reductionType xReduction
          yReduction = visit y notEtaFlag
          yReductionTerm = reductionTerm yReduction
          yReductionType = reductionType yReduction

visit (Var v) _ = Reduction (Var v) ""

visit (Abstraction x (Application z (Var w))) notEtaFlag
    | x == w && not (isFreeIn x z) && not notEtaFlag = Reduction z "eta"
    | otherwise = Reduction (Abstraction x redTerm) redType
    where reduction = visit (Application z (Var w)) notEtaFlag
          redTerm = reductionTerm reduction
          redType = reductionType reduction

visit (Abstraction x y) notEtaFlag = Reduction (Abstraction x yReductionTerm) yReductionType
    where yReduction = visit y notEtaFlag
          yReductionTerm = reductionTerm yReduction
          yReductionType = reductionType yReduction
