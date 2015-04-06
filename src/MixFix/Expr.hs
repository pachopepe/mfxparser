

module MixFix.Expr where

import Control.Applicative ((<$>),liftA2)
import Data.Monoid (mappend,mempty)
import Data.List ((\\),nub)

-- | Mixfix data type representation
-- Part is a syntax part
-- Place is an expression place
data MixFix = Place -- ^ represents an expression place
            | Part String -- ^ represents a fixed part 
            deriving (Eq,Ord)

instance Show MixFix where
     show Place = "_"
     show (Part s) = s
     showList es = ((concat . map show $ es)++)

type Identifier = String

-- | Expression data type
data Expr = Con  Identifier  -- ^ Constant
          | Var  Identifier  -- ^ Variable
	  | MVar Identifier  -- ^ Meta variable
          | Conj Expr [([MixFix],[Expr],Expr)]  -- ^ Operator conjunction
	  | Op   [MixFix] -- ^ Operator mixfix sintax
                 [Expr]   -- ^ expression parts, fill the mixfix places
          | Quan [MixFix] -- ^ Quantifier mixfix sintax
                 [Identifier] -- ^ binding variables
                 Expr     -- ^ range expression
                 Expr     -- ^ body expression 
          | Subs Expr Sust -- ^ Substitucion.
          | Comp -- ^ Container comprehension
                 [MixFix]  -- ^ Container mixfix sintax
                 [Identifier] -- ^ binding variables
                 Expr      -- ^ range expresion
                 Expr      -- ^ body expresion
          | Ext -- ^ Container by extension
                [MixFix]  -- ^ extension minfix sintax
                [Expr]    -- ^ expresion elements
          | Tup  [Expr]   -- ^ tuple
          | Paren Expr     -- ^ expresion with parenthesis
          deriving (Eq,Show)

-- | Substitution
type Sust = [(Identifier, Expr)]

-- | Empty Substitucion
nullSust :: Sust
nullSust = []

-- | Substitution composition
infixr 4 @@
(@@) :: Sust -> Sust -> Sust
s1 @@ s2 = [ (v,apply [] s2 e) | (v,e) <- s1 ] 
           ++ [ (v,e) | (v,e) <- s2, v `notElem` map fst s1 ]

-- | Expression and substitution management
class Exprs e where
    -- | Apply the substitution to expression.
  -- The bounded variables are not replaced
    apply :: [Identifier] -> Sust -> e -> e
    -- Obtiene las variables libres de una expresion
    fv    :: [Identifier] -> e -> ([Identifier],[Identifier])
    -- Obtiene las variables utilizadas de una expresion
    vars  :: e -> ([Identifier],[Identifier])

instance Exprs Expr where
    apply bv s (Var v) = bindVar bv s v Var 
    apply bv s (MVar v) = case bindVar bv s v MVar of
                               MVar v' -> if v == v' 
                                          then Subs (MVar v) s
                                          else MVar v'
                               e -> e 
    apply bv s (Op op es) = Op op (apply bv s es) 
    apply bv s (Subs e s1) = Subs e' s1'
                        where s1' = [ (v,e1) | (v,e1) <- s1,e1 <- [apply bv s e],Var v /= e1 ] 
                              e' = apply bv [ (v,e) | (v,e) <- s, v `notElem` map fst s1 ] e
    apply bv s (Quan op vs e1 e2) = Quan op vs (apply bv' s e1) (apply bv' s e2)
                                  where bv' = vs++bv
    apply bv s (Comp cl vs e1 e2) = Comp cl vs (apply bv' s e1) (apply bv' s e2)
                                  where bv' = vs++bv
    apply bv s (Ext cl es) = Ext cl (apply bv s es)
    apply bv s (Tup es) = Tup (apply bv s es)
    apply bv s e = e

    fv bv (Var v) = if v `elem` bv 
                    then ([],[])
                    else ([v],[])
    fv bv (MVar v) = ([],[v])
    fv bv (Op op es) = fv bv es
    fv bv (Subs e s1) = nub <$> fv bv ran `mappend` fv (dom++bv) e
                      where (dom,ran) = unzip s1 
    fv bv (Quan op vs e1 e2) = fv bv' [e1, e2]
                               where bv' = vs ++ bv
    fv bv (Comp cl vs e1 e2) = fv bv' [e1,e2]
                               where bv' = vs ++ bv
    fv bv (Ext cl es) = fv bv es
    fv bv (Tup es) = fv bv es
    fv bv e = ([],[]) 

    vars (Var v) = ([v],[])
    vars (MVar v) = ([],[v])
    vars (Op op es) = vars es
    vars (Subs e s1) = nub <$> (vars e `sub` (dom,[]) `mappend` vars ran)
                     where (dom,ran) = unzip s1
                           sub = liftA2 (\\) 
    vars (Quan op vs e1 e2) = vars [e1,e2]
    vars (Comp cl vs e1 e2) = vars [e1,e2]
    vars (Ext cl es) = vars es
    vars (Tup es) = vars es
    vars e = ([],[]) 

instance Exprs a => Exprs [a] where
    apply bv s = map (apply bv s)
    fv bv es = nub <$> (foldl mappend ([],[]) . map (fv bv) $ es)
    vars es = nub <$>  (foldl mappend ([],[]) . map vars $ es)

-- Funcion auxiliar para sustituye la variable vn si esta libre 
-- (no esta acotada, no aparece en la lista bv) 
bindVar :: [Identifier] -> Sust -> Identifier -> (Identifier -> Expr) -> Expr
bindVar bv s vn ctr = if vn `elem` bv
                      then ctr vn
                      else case lookup vn s of
                                Nothing -> ctr vn
                                Just e  -> e

