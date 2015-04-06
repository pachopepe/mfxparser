
module MixFix.Dic where

import MixFix.Expr

-- import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)

-- | Operator Properties
data Property = Assoc -- ^ The operator is associative
              | AssocLeft -- ^ The operator is left associative
              | AssocRight -- ^ The operator is right associative
              | Symmetric -- ^ The operator is symmetric
              | Idempotent -- ^ The operator is idempotent
              | Ident String -- ^ The operator has identity u
              | IdentLeft String -- ^ The operator has left identity u
              | IdentRight String -- ^ The operator has right identity u
              | Conjuntive -- ^ The operator is conjuntive
                           -- ^ x `op` y `op` z = (x `op` y) /\\ (x `op` z)  
              deriving (Show,Eq)

data Operator a = Operator { mixfix :: [MixFix], 
                             properties :: [Property], 
                             precedence :: Int, 
                             info :: a }
                | Alias  { mixfix :: [MixFix] , alias :: [MixFix] }
                | Closed { mixfix :: [MixFix], info :: a }
                deriving (Show,Eq)           

getMixFixName :: [MixFix] -> String
getMixFixName = concat . map strPart 
              where strPart Place = "_"
                    strPart (Part s) = s

unMixFixName :: [Char] -> [MixFix]
unMixFixName [] = []
unMixFixName ('_':es) = Place : unMixFixName es
unMixFixName es = case span (/= '_') es of
                       (nm,es') -> Part nm : unMixFixName es'

getOpName :: Operator a -> String
getOpName = getMixFixName . mixfix

maxPrec :: Int
maxPrec = 2^(62::Int)

getPrecedence :: Operator a -> Int
getPrecedence (Operator _ _ p _) = p
getPrecedence _ = maxPrec

isAssoc :: Operator a -> Bool
isAssoc (Operator _ props _ _ ) = Assoc `elem` props
isAssoc _ = False

isAssocLeft :: Operator a -> Bool
isAssocLeft (Operator _ props _ _ ) = AssocLeft `elem` props
isAssocLeft _ = False

isSymmetric :: Operator a -> Bool
isSymmetric (Operator _ props _ _ ) = Symmetric `elem` props
isSymmetric _ = False

isAssocRight :: Operator a -> Bool
isAssocRight (Operator _ props _ _ ) = AssocLeft `elem` props
isAssocRight _ = False

isConjuntive :: Operator a -> Bool
isConjuntive (Operator _ props _ _ ) = Conjuntive `elem` props
isConjuntive _ = False

hasIdent :: Operator a -> Bool
hasIdent (Operator _ props _ _ ) = any hId props
                             where hId (Ident _) = True
                                   hId _ = False
hasIdent _ = False

mxArity :: [MixFix] -> Int
mxArity = length . filter (== Place)

arity :: Operator a -> Int
arity = mxArity . mixfix

isMixFixPrefix :: [MixFix] -> Bool
isMixFixPrefix (Part _:_) = True
isMixFixPrefix _ = False

isAbelianMonoid :: Operator a -> Bool
isAbelianMonoid op = all ($ op) [isAssoc, isSymmetric,hasIdent]  

opKeyName :: Operator a -> [MixFix]
opKeyName op = mxKeyName . mixfix $ op 

mxKeyName :: [MixFix] -> [MixFix]
mxKeyName parts = take (if isMixFixPrefix parts then 1 else 2) parts

type Assump a = (String,a)

data Dictionary a = Dictionary 
                     { operator   :: Map [MixFix] [Operator a] -- Operadores prefijos
                     , var        :: [Assump a] -- variables globales
                     , mvar       :: [Assump a] -- meta variables
                     , constant   :: [Assump a] -- constantes
                     , conjuntive :: [[String]] -- compatibles por conjuntividad
                                                -- deben estar ordenados
                                                -- mas debil al mas fuerte
                                                -- ejemplo =, <=, <
                     -- , tVarId  :: Integer -- Siguiente identificador libre de variable de tipo
                     }
                     deriving (Show,Eq)

addOperator :: Operator a -> Dictionary a -> Dictionary a 
addOperator op d = d { operator = Map.insertWith appendOrd (opKeyName op) [op] (operator d) } 
             where appendOrd [op'] ops = insertOrd (\op1 op2 -> mixfix op1 > mixfix op2) op' ops
                   -- appendOrd ops ops' = error ("paso mas operadores "++show ops')

insertOrd :: (a -> a -> Bool) -> a -> [a] -> [a]
insertOrd cmp x [] = [x]
insertOrd cmp x (y:ys) = if cmp x y 
                         then x:y:ys
                         else y:insertOrd cmp x ys


{-
addOperator :: Operator -> Dictionary -> Dictionary 
addOperator op d = d { operator = Map.insert (opKeyName op) op (operator d) } 
-}

assumpNames :: [Assump a] -> [String]
assumpNames = map fst

addVar :: Assump a -> Dictionary a -> Dictionary a 
addVar v d = d { var = v : var d }

addCst :: Assump a -> Dictionary a -> Dictionary a
addCst v d = d { constant = v : constant d }

addMVar :: Assump a -> Dictionary a -> Dictionary a 
addMVar v d = d { mvar = v : mvar d }


addConjuntive :: [String] -> Dictionary a -> Dictionary a 
addConjuntive cn d = d { conjuntive = cn : conjuntive d }

origOperator :: Dictionary a -> Operator a -> Operator a 
origOperator dic al@(Alias _ _) = origOperator' al
            where ops kn = (Map.findWithDefault [] kn (operator dic)) 
                  origOperator' cl@(Closed _ _) = cl
                  origOperator' op@(Operator _ _ _ _) = op
                  origOperator' (Alias mx mx') = 
                       case Data.List.find ((== mx') . mixfix) (ops kn) of
                            Nothing -> error $ "origOperator: "++show mx++" invalid operator"
                            Just op -> origOperator' op
                       where kn = mxKeyName mx'
origOperator dic orig = orig


toOperator dic al@(Alias mx _) = 
           case origOperator dic al of
                (Closed _ sig) -> Closed mx sig
                (Operator _ props prec sig) -> Operator mx props prec sig
                _ -> error ("toOperator: operator '"++ show mx
                            ++"' not found")            
toOperator dic other = other

getOperator :: Dictionary a -> [MixFix] -> Operator a
getOperator _ [] = error "getOperator: no operator given"
getOperator dic mx = toOperator dic (Alias mx mx)

getOperators :: Dictionary a -> [MixFix] -> [Operator a]
getOperators dic key = map (toOperator dic) (Map.findWithDefault [] key (operator dic))

addProperty :: Dictionary a -> [MixFix] -> Property -> Dictionary a
addProperty dic mx p = if p `notElem` properties op
                       then dic { operator = Map.adjust replaceOp 
                                                 (opKeyName op) (operator dic) }
                       else dic
                     where op = origOperator dic (Alias mx mx)
                           op' = op { properties = p:properties op }
                           replaceOp [] = []
                           replaceOp (op'':ops) = if mixfix op'' == mixfix op'
                                                  then op':ops
                                                  else op'':replaceOp ops 


