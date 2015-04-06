
module Example where

import MixFix.Expr
import MixFix.Parser
import MixFix.Dic
import qualified Data.Map as Map
import Text.Parsec.Error (ParseError(..))


(=>>) :: String -> a -> Assump a
(=>>) = (,)

infoUnInt = ()
infoOpNum = ()
infoOpRel = ()
infoNum = ()
infoBool = ()
infoFun = ()
infoCont = ()
infoMVar = ()
infoIf = ()

initialState' :: Dictionary ()
initialState' = 
   -- Operators and container list
       addOperator (Closed [Part "{",Place,Part "}"] infoCont )
       .addOperator (Operator [Place,Part "/\\",Place] [Assoc,Ident "True"] 30 infoOpNum)
       .addOperator (Operator [Place,Part "\\/",Place] [Assoc,Ident "False"] 20 infoOpNum)
       .addOperator (Operator [Place,Place] [AssocLeft] maxPrec infoOpNum)
       .addOperator (Operator [Place,Part ".",Place] [Assoc] maxPrec infoOpNum)
       .addOperator (Operator [Place,Part "!"] [Assoc] 100 infoUnInt)
       .addOperator (Operator [Place,Part "^",Place] [AssocRight] 80 infoOpNum)
       .addOperator (Operator [Place,Part "+",Place] [Assoc,Symmetric,Ident "0"] 80 infoOpNum)
       .addOperator (Operator [Place,Part "*",Place] [Assoc,Symmetric,Ident "1"] 100 infoOpNum)
       .addOperator (Operator [Place,Part "<",Place] [Conjuntive] 40 infoOpRel)
       .addOperator (Operator [Place,Part "<=",Place] [Conjuntive] 40 infoOpRel)
       .addOperator (Operator [Place,Part "==",Place] [Conjuntive] 10 infoOpRel)
       .addOperator (Operator [Place,Part "=",Place] [Conjuntive] 40 infoOpRel)
       .addOperator (Operator [Place,Part ">",Place] [Conjuntive] 40 infoOpRel)
       .addOperator (Operator [Place,Part ">=",Place] [Conjuntive] 40 infoOpRel)
       .addOperator (Operator [Part "if",Place,Part "then",Place,Part "else",Place] [] 0 infoIf)
       --, ("H",[Prefix [Part "H",Place,Place] tsOpInt])
       $ Dictionary Map.empty  
         ["f" =>> infoFun,"g" =>> infoFun,"b" =>> infoBool, "p" =>> infoBool,"q" =>> infoBool,"r" =>> infoBool, "n" =>> infoNum, "m" =>> infoNum]
         ["E" =>> infoMVar,"R" =>> infoMVar,"Q" =>> infoMVar]
         [ "true" =>> infoBool,"false" =>> infoBool
       , "0" =>> infoNum,"1" =>> infoNum,"2" =>> infoNum,"3" =>> infoNum,"4" =>> infoNum,"5" =>> infoNum
         ]  
         []
                  
initialState :: Dictionary ()
initialState = 
   addOperator (Operator [Part "if",Place,Part "then",Place,Part "else",Place,Part "fi"] [] 0 infoIf) 
   . addOperator (Closed [Part "〈",Place,Part "〉"] infoCont)
   . addOperator (Operator [Part "if",Place,Part "then",Place] [] 0 infoIf) 
   -- . addOperator (Operator [Place,Place,Part "K"] [Assoc] maxPrec infoOpInt)
   -- . addOperator (Operator [Part "H",Place,Place] [Assoc] maxPrec infoOpInt)
   -- . (\d -> addProperty d [Part "All",Place,Place] Symmetric)
   . addOperator (Alias [Part "All",Place,Place] [Place,Part "/\\",Place])
   -- . addOperator (Operator [Place,Part "+",Place,Part "-",Place] [] 90 tsOpInt) 
   $ initialState'

{-
typeState :: Dictionary ()
typeState =
     addOperator (Operator [Place,Part "->",Place] [] 0 infoOpRel)
     . 
-}

-- To run some Examples:
--    runParse initialState (flip parseExpr (0,"")) "if b then if p then q else r"
--    runParse initialState (flip parseExpr (0,"")) "p /\\ q \\/ r"
--    runParse initialState (flip parseExpr (0,"")) "p \\/ q /\\ r"
--    runParse initialState (flip parseExpr (0,"")) "p[q := r]"

ifAmbig :: Either Text.Parsec.Error.ParseError Expr
ifAmbig = runParse initialState (flip parseExpr (0,"")) "if b then if p then q else r"

boolExpr1 :: Either Text.Parsec.Error.ParseError Expr
boolExpr1 = runParse initialState (flip parseExpr (0,"")) "p /\\ q \\/ r"

boolExpr2 :: Either Text.Parsec.Error.ParseError Expr
boolExpr2 = runParse initialState (flip parseExpr (0,"")) "p \\/ q /\\ r"

sustExpr :: Either Text.Parsec.Error.ParseError Expr
sustExpr  = runParse initialState (flip parseExpr (0,"")) "p[q := r]"

parse :: String -> Either Text.Parsec.Error.ParseError Expr
parse  = runParse initialState (flip parseExpr (0,"")) 

