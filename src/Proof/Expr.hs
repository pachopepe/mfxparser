

module Proof.Expr where

import MixFix.Expr
import MixFix.Dic 
import Data.List (intersperse,span,delete)

cstTrue :: Expr 
cstTrue = Con "True"

isCstTrue :: Expr -> Bool
isCstTrue c = c == cstTrue

isSimpleExpr :: Expr -> Bool
isSimpleExpr (Var _) = True 
isSimpleExpr (MVar _) = True 
isSimpleExpr (Con _) = True 
isSimpleExpr _ = False 


showExpr :: Dictionary -> Int -> Expr -> String
showExpr d p e = showExpr' d p False e ""

showSpace :: ShowS
showSpace = showString " "

compose :: [(a->a)] -> (a -> a)
compose = foldr (.)  id

-- | showExpr' d p c e
-- | precedencia del operador que lo contiene
-- | conjuntividad del operador que lo contiene (True es conjuntivo, False no es conjuntivo)
showExpr' :: Dictionary -> Int -> Bool -> Expr -> ShowS
showExpr' _ _ _ (Con c) = showString c
showExpr' _ _ _ (Var v) = showString v
showExpr' _ _ _ (MVar v) = showString v
showExpr' d p _ (Subs e ss) = (showExpr' d (maxPrec + 1) False e) . showSpace . showSust d ss
showExpr' d p _ (Quan op es range body) = 
    case op of
         [Place,Part sOp,Place] -> showQ sOp
         [Part sOp,Place,Place] -> showQ sOp
         [Place,Place,Part sOp] -> showQ sOp
         _ -> error ("invalid quantifier operator '"++ show op ++"'")
         where showQ s = showParen True (
                                   showString s 
                                   . showSpace 
                                   . showBind d es range body True)
showExpr' d p _ (Comp [Part open,_,Part close] es range body) = 
          showString open . showBind d es range body False . showString close
showExpr' d p _ (Ext [Part open,_,Part close] es) = 
                                        showString open 
                                        . showExprList d es 
                                        . showString close
showExpr' d p _ (Tup es) = showParen True $ showExprList d es   
showExpr' d p cnj (Op parts es) = 
            showParen (p > pOp && not (cnj && cnjOp)) (showWithParts' d cnjOp pOp parts' es)
    where op = getOperator d parts
          pOp = getPrecedence op
          cnjOp = isConjuntive op
          parts' = if ar < n -- Operador asociativo aplanado
                   then if last parts == Place
                        then concat (replicate np (init parts)) ++ parts
                        else parts ++ concat (replicate np (tail parts))
                   else parts
                 where n = length es
                       ar = arity op
                       np = (n - ar) `div` (ar - 1)
showExpr' d p _ (Conj e ((mx,mid,en):exs)) = 
                                   showParen (p > pOp) $ showWithParts' d True pOp parts exps
                             where mxs' = map (\(mx',_,_) -> mx') exs
                                   parts = mx ++ concat (map elmFirstPlace mxs')
                                   exps = e:mid++[en] ++ concat (map (\(_,mid',en') -> mid' ++ [en']) exs)
                                   pOp = maximum (map (getPrecedence . getOperator d) (mx:mxs'))
                                   elmFirstPlace mx = delete Place mx

showWithParts' :: Dictionary -> Bool -> Int -> [MixFix] -> [Expr] -> ShowS
showWithParts' _ _ _ [] [] = id
-- Ultima expresion del operador
showWithParts' d cnj pOp [Place] [e] = showExpr' d (pOp + 1) cnj e
-- Al menos dos puestos seguidos
showWithParts' d cnj pOp ps@(Place : Place :ps') es = 
                         compose (intersperse showSpace . map showPlace $ places)
                         . if n == m
                           then id
                           else showSpace . showWithParts' d cnj pOp (drop n ps) (drop n es)
                     where places = takeWhile ((==Place). fst) (zip ps es)
                           n = length places
                           m = length ps
                           showPlace (_,e) = showExpr' d (maxPrec + 1) False e 
showWithParts' d cnj pOp (Place : ps) (e:es) = showExpr' d (pOp + 1) cnj e 
                                       . showSpace
                                       . showWithParts' d cnj pOp ps es
-- Hay contexto no se necesitan parentesis en el puesto
showWithParts' d cnj pOp (Part s : Place : Part t :ps) (e:es) = 
                          showString s . showSpace .
                          showExpr' d 0 False e . showSpace .
                          showString t . showSpace .
                          showWithParts' d cnj pOp ps es
-- Fin de un operador posfijo, no hay que colocar el espacio final
showWithParts' _ _ _ [Part s] [] = showString s
showWithParts' d cnj pOp (Part s : ps) es = 
                          showString s . showSpace .
                          showWithParts' d cnj pOp ps es

showExprList :: Dictionary -> [Expr] -> ShowS
showExprList d = compose . intersperse (showString ",") . map (showExpr' d 0 False)

showBind :: Dictionary -> [Identifier] -> Expr -> Expr -> Bool -> ShowS
showBind d es range body showBody = showIdentifiers es . showString " |" . showEnd
     where showEnd = if not showBody  
                     then case (es,body) of
                               ([v],Var v') | v == v' ->
                                        if isCstTrue range 
                                        then showExpr' d 0 False range
                                        else showRange range
                               _ -> showRange range . showString ": " 
                                                    . showExpr' d 0 False body
                     else showRange range . showString ": " . showExpr' d 0 False body
           showRange range = if isCstTrue range
                             then id
                             else showSpace . showExpr' d 0 False range . showSpace

showIdentifiers :: [String] -> ShowS
showIdentifiers = compose . intersperse (showString ",") . map showString

showSust :: Dictionary -> [(String, Expr)] -> ShowS
showSust d ss = showString "[" 
                . showIdentifiers vs
                . showString " := " 
                . showExprList d es 
                . showString "]" 
         where (vs,es) = unzip ss 

associativity :: Dictionary -> Expr -> Expr
associativity dic (Con c) = Con c
associativity dic (Var v) = Var v
associativity dic (MVar v) = MVar v
associativity dic (Op mx es) =
                    if isAssoc . getOperator dic $ mx  
                    then Op mx $ do e <- map (associativity dic) es
                                    case e of
                                         (Op mx' es') | mx == mx' -> es'
                                         e' -> return e' 
                    else Op mx $ map (associativity dic) es
associativity dic (Quan mx xs r e) = Quan mx xs (associativity dic r) (associativity dic e)
associativity dic (Subs e ss) = Subs (associativity dic e) 
                                     [(v,associativity dic e') | (v,e') <- ss] 
associativity dic (Comp mx xs r e) = Comp mx xs (associativity dic r) (associativity dic e) 
associativity dic (Ext mx es) = Ext mx (map (associativity dic) es)
associativity dic (Tup es) = Tup (map (associativity dic) es)

group :: Int -> [a] -> [[a]]
group n [] = []
group n es = as:group n bs
           where (as,bs) = splitAt n es

splitArgs mx [] = error ("Invalid number of arguments in conjuntive operator '"++show mx ++"'")
splitArgs mx [e] = error ("Invalid number of arguments in conjuntive operator '"++show mx ++"'")
splitArgs mx (e0:es) = (e0,init es,last es)

convConjuntive :: Dictionary -> Expr -> Expr
convConjuntive dic (Con c) = Con c
convConjuntive dic (Var v) = Var v
convConjuntive dic (MVar v) = MVar v
convConjuntive dic (Op mx []) = Op mx []
convConjuntive dic (Op mx [e]) = Op mx [e]
convConjuntive dic (Op mx es) = 
                    if isConjuntive . getOperator dic $ mx   
                    then case splitArgs mx es' of
                              (Conj e0 exs0,mid,Conj en exsn) -> Conj e0 (exs0++((mx,mid,en):exsn))
                              (e0,mid,Conj en exsn) -> Conj e0 ((mx,mid,en):exsn)
                              (Conj e0 exs0,mid,en) -> Conj e0 (exs0 ++ [(mx,mid,en)])
                              (e0,mid,en) -> Conj e0 [(mx,mid,en)]
                    else Op mx es'
                  where es' = map (convConjuntive dic) es                        
convConjuntive dic (Quan mx xs r e) = Quan mx xs (convConjuntive dic r) (convConjuntive dic e)
convConjuntive dic (Subs e ss) = Subs (convConjuntive dic e) 
                                     [(v,convConjuntive dic e') | (v,e') <- ss] 
convConjuntive dic (Comp mx xs r e) = Comp mx xs (convConjuntive dic r) (convConjuntive dic e) 
convConjuntive dic (Ext mx es) = Ext mx (map (convConjuntive dic) es)
convConjuntive dic (Tup es) = Tup (map (convConjuntive dic) es)
