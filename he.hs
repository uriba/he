import Control.Arrow(second)
import qualified Data.Map as Map
import Data.Map ( Map )


type TermKey = Int
type VarKey = Int
type VarMap = Map VarKey Var
type TermMap = Map TermKey Term
type Var = ()
type DataCtr = String

data Term = Lambda { lambdaVar :: VarKey, lambdaBody :: TermRef } |
			Apply { applyFunc :: TermRef, applyArg :: TermRef } |
			ReadVar { readVar :: VarKey } |
			Case { caseExpr :: TermRef, cases :: [(DataCtr, TermRef)] }

lambda :: VarKey -> TermKey -> Term
lambda vk tk = Lambda vk (TermKey tk)

apply af aa = Apply (TermKey af) (TermKey aa)

case' ce cs = Case (TermKey ce) ((map . second) TermKey cs)

data TermRef = TermKey TermKey | Builtin String

varMap :: VarMap
varMap = Map.fromList [
	( 0,  () )	
	  ]
termMap :: TermMap
termMap = Map.fromList [
	 ( 0, lambda 0 1 )
	,( 1, Case (TermKey 2) [("True", Builtin "1"), ("False", TermKey 4)])
	,( 2, Apply (TermKey 5) (Builtin "0") ) -- x==0
	,( 4, apply 7 8 ) -- * (fac (x-1)) x
	,( 5, Apply (Builtin "(==)") (TermKey 8) ) -- == x
	,( 7, Apply (Builtin "(*)") (TermKey 9) ) -- * (fac (x-1))
	,( 8, ReadVar 0 ) -- x
	,( 9, apply 0 10 ) -- fac (x-1)
	,( 10, Apply (TermKey 11) (Builtin "1") ) -- x-1
	,( 11, Apply (Builtin "(-)") (TermKey 8) )
	]

varName :: VarKey -> String
varName vk = "var_" ++ show vk

termName :: TermKey -> String
termName tk = "term_" ++ show tk

termRefString :: TermRef -> String
termRefString (TermKey tk) = termName tk
termRefString (Builtin b) = b

pp :: TermMap -> String
pp = unlines . Map.elems . Map.mapWithKey termDefinition 
  where
 	termDefinition tk t = termName tk ++ " = " ++ termString t
	termString (Lambda lv lb) = "( \\" ++ varName lv ++ " -> " ++ termRefString lb ++ " )"
	termString (Apply af aa) = termRefString af ++ " " ++ termRefString aa
	termString (ReadVar vk) = varName vk
	termString (Case ce cs) = "(case " ++ termRefString ce ++ " of {" ++ concatMap makeCaseStr cs ++ " } )"
	makeCaseStr (dataCtr, tr) = dataCtr ++ " -> " ++ termRefString tr ++ " ; "

main = do
	putStrLn $ pp termMap

{--------------------------------------
fac = \x -> p
p = case x==0 of
      True  -> 1
	  False -> fac (x-1) * x
---------------------------------------}
--
