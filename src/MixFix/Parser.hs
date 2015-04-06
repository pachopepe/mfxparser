{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

-- Francisco Cháves
--
-- This is a mixfix parser work on with a calculational expressions for quantifiers and containers 
-- (http://www.cs.utexas.edu/users/EWD/transcriptions/EWD13xx/EWD1300.html)
--
-- The precedence parser is based on Parsing expressions by precedence climbing
-- http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing/
-- http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
-- http://antlr.org/papers/Clarke-expr-parsing-1986.pdf
--
-- I added backtracking by expresion length with monadic parsers to solve ambiguities;
--    by example, if we have to prefix operators:
--    if_then_ and if_then_else, and the expresion if e0 then if e1 then e2 else e3, it is analised as
--       the expresion if e0 (if e1 then e2 else e3)
--
 
module MixFix.Parser where

import Data.List
import qualified Data.Map as Map

import Text.Parsec
import Control.Monad
import Control.Monad.Identity
import Text.Parsec.Pos

import MixFix.Lexer
import MixFix.Expr
import MixFix.Dic
-- import Proof.Expr

cstTrue :: Expr
cstTrue = Con "True"

-- | Comma
comma :: Token
comma = Token SYMBOL ","

bar :: Token
bar = Token SYMBOL "|"

barColon :: Token
barColon = Token SYMBOL "|:"

colon :: Token
colon = Token SYMBOL ":"

-- | Parser data type
-- position and operators dictionary
type TParser b a = ParsecT [TokenPos] (Dictionary b) Identity a

type Env = [Identifier]

-- | Delta Iverson: boolean to a number
iv :: Num a => Bool -> a
iv p = if p then 1 else 0

-- | 'getDictionary' obtains the dictionary
getDictionary :: TParser a (Dictionary a)
getDictionary = getState

-- | 'setDictionary' sets a new dictionary
setDictionary :: Dictionary a -> TParser a ()
setDictionary = putState

-- | @'updateDictionary' f@ update the dictionary with the function f
updateDictionary :: (Dictionary a -> Dictionary a) -> TParser a ()
updateDictionary = updateState

-- | Advance the token position 
advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

-- | @'sat' p@ reads a new token and check if satisfies @p@
sat :: (Token -> Bool) -> TParser a Token
sat p = tokenPrim show
                  advance
                  (\(c,_) -> if p c then Just c else Nothing)

-- | @'acceptTk' t@ accept token @t@
acceptTk :: Token -> TParser a ()
acceptTk tt = sat (== tt) >> return ()  
            <?> "Assert fail: expected '"++ show tt ++ "'"          

-- | @'parens' p@ parse @p@ between parenthesis
parens :: TParser b a -> TParser b a
parens p =  between (acceptTk $ Token LEFT_PAREN "(") (acceptTk $ Token RIGHT_PAREN ")") p

-- | @'cntParens' left right p@ parse @p@ between delimiters @left@ and @right@ 
cntParens :: String -> String -> TParser b a -> TParser b a
cntParens left right p = between (acceptTk (Token OPEN left))
                                 (acceptTk (Token CLOSE right)) p

-- | @'parseAtom' env@ atomic parser expresiones (include parenthesized expresions)
--   and prefix operators ,
-- @env@ is the variables envirenment
parseAtom :: Env -> TParser b Expr
parseAtom env = do 
          (Token tk s,_) <- lookAhead anyToken
          case tk of 
               LEFT_PAREN -> parens (parseQuantifier env <|> parseTuple env)
               OPEN -> parseContainer env s
               NUMBER -> anyToken >> parseCstVar env s
               NAME -> anyToken >> (parsePrefixOp env s <|> parseCstVar env s)
               SYMBOL -> anyToken >> (parsePrefixOp env s <|> parseCstVar env s) 
               _ -> fail ("parseAtom: invalid token '"++show tk++" "++ s ++"'")

-- | @'parseMVar' v@ parser de una meta variable, busca su informacion en 
-- las meta variables del diccionario 
parseCstVar :: Env -> String -> TParser b Expr
parseCstVar env v = do d <- getDictionary   
                       if v `elem` env 
                       then if v `elem` assumpNames (mvar d)
                            then return $ MVar v
                            else if v `elem` assumpNames (constant d)
                                 then return $ Con v
                                 else return $ Var v
                       else fail ("Undefined variable or constant '"++v++"'")

parseExpr' :: Env -> TParser b Expr
parseExpr' env = parseExpr env (0,"")

-- | @'parseExpr' env@ parser para las expresiones, @env@ 
--   es el ambiente de variables 
parseExpr :: Env -> (Int,String) -> TParser b Expr
parseExpr env (prec,lOp) = do
          e0 <- parseAtom env
          parseInfix env e0 (prec,lOp)

-- | @'parseInfix' env@ parser para las expresiones infijas, 
--   @e0@ es la expresion que se proceso
--   @(prec,lOp)@ es la expresion contenedora 
--   @env@ es el ambiente de variables 
parseInfix :: Env -> Expr -> (Int,String) -> TParser b Expr
parseInfix env e0 (prec,lOp) =
      do  (Token tk s,_) <- lookAhead anyToken
          case tk of 
                NAME  -> parseInfixOp env e0 (prec,lOp) [Place,Part s] 
                SYMBOL  -> parseInfixOp env e0 (prec,lOp) [Place,Part s]
                OPEN -> parseInfixOp env e0 (prec,lOp) [Place,Part s]  
                _ -> parseInfixOp env e0 (prec,lOp) [Place,Place] 

-- | @'parseInfixOp' env kn@ parser de un operador Infijo o posfijo  
parseInfixOp :: Env -> Expr -> (Int,String) -> [MixFix] -> TParser b Expr
parseInfixOp env e0 (prec,lOp) kn = 
  do d <- getDictionary
     let opsEmpty = getOperators d [Place,Place]
         opsKn = getOperators d kn 
     case [ op | op <- (opsKn ++ opsEmpty), getPrecedence op >= prec ] of
          [] -> return e0
          ops -> (choice . map (try . parseGenericInfix env e0 (prec,lOp)) $ ops)
                   <|> do guard (kn == [Place,Part "["])
                          e' <- try (parseSust env e0)
                          parseInfix env e' (prec,lOp)
                   <|> return e0
                           
parseGenericInfix :: Env -> Expr -> (Int,String) -> Operator b -> TParser b Expr 
parseGenericInfix env e (prec,_) op@(Operator parts _ precOp _) =
      do es <- parseParts env (prec',opn) parts
         parseInfix env (Op parts (e:es)) (prec,opn)
   where opn = getOpName op
         prec' = precOp + iv (isAssocLeft op || isAssoc op || isConjuntive op)
parseGenericInfix _ _ _ _ = error ("Invalid call to parseGenericInfix")

isPostfixParts :: [MixFix] -> Bool
isPostfixParts parts = last parts /= Place

parseParts :: Env -> (Int,String) -> [MixFix] -> TParser b [Expr]
parseParts env op parts = pParts (tail parts)
        where pParts [] = return []
              pParts [Place] = do e <- parseExpr env op
                                  return [e]
              pParts (Place:Place:ps) = do e <- parseAtom env
                                           es <- pParts (Place:ps)
                                           return (e:es)
              pParts (Place:ps) = do e <- parseExpr' env
                                     es <- pParts ps
                                     return (e:es)
              pParts (Part s:ps) = do _ <- sat (isPart s)
                                      es <- pParts ps 
                                      return es
              isPart s (Token NAME t)   = s == t
              isPart s (Token SYMBOL t) = s == t
              isPart _ _ = False

-- | @'parsePrefixOp' env nm@ parser de un operador prefijo  
parsePrefixOp :: Env -> String -> TParser b Expr
parsePrefixOp env nm = 
     do d <- getDictionary
        choice . map (try . parseGenericPrefix env) $ getOperators d [Part nm]
                           
parseGenericPrefix :: Env -> Operator b -> TParser b Expr 
parseGenericPrefix env op@(Operator parts _ precOp _) =
     do es <- parseParts env (precOp,opn) parts
        return $ (Op parts es)
   where opn = getOpName op
parseGenericPrefix _ _ = error ("Invalid call to parseGenericPrefix")

-- | @'parseQuantifier' env@ parser de un cuantificador  
--   @env@ es el ambiente de variables locales
parseQuantifier :: Env -> TParser b Expr
parseQuantifier env = do op <- parseQuantifierOp
                         -- TODO verificar que op sea un monoide abeliano
                         (vs,range,body) <- parseBind env
                         return (Quan op vs range body)

parseQuantifierOp :: TParser b [MixFix]
parseQuantifierOp = do 
          (tk,_) <- lookAhead anyToken
          case tk of
               (Token NAME n) -> getQOp n
               (Token SYMBOL s) -> getQOp s
               _ -> fail ("The token '"++ show tk ++"' is not a quantifier operator")
        where getQOp nm = do
                 d <- getDictionary
                 case [ op | op <- getOperators d [Place,Part nm]++getOperators d [Part nm]
                           , isAbelianMonoid op] of
                      [] -> fail ("Not a valid quantifier operator '"++nm++"' provided")
                      (op:_) -> do _ <- anyToken 
                                   return $ mixfix op
 

-- | @'parseBind' env@ parser de la parte comun de un cuantificador y un contenedor por
--   compresion, @env@ es el ambiente de variables locales  
parseBind :: Env -> TParser b (Env,Expr,Expr)
parseBind env = do vs <- parseLocalVarList
                   let env' = vs ++ env
                   (r,hasColon) <- (acceptTk barColon >> return (cstTrue,True)) <|>
                                   (acceptTk bar >> do { r <- parseRange env';
                                                         return (r,False) } )
                   e <- if length vs == 1 && not hasColon 
                        then parseOptionalBody env' (Var $ head vs)
                        else do when (not hasColon) (acceptTk colon)
                                parseBody env'
                   return (vs,r,e)

parseLocalVarList :: TParser b Env
parseLocalVarList = do vs <- sepBy parseLocalVar (acceptTk comma)
                       case vs \\ nub vs of
                            [] -> return vs
                            xs -> fail ("Duplicate variable(s) definition: '"
                                       ++ show (intersperse "', '" xs) ++"'")

parseLocalVar :: TParser b String
parseLocalVar = do (Token NAME nm) <- sat isTkName
                   -- TODO de donde toma los valores la variable <-
                   -- TODO Parser del tipo de la variable
                   {-
                   do acceptTk colon
                      return nm
                    <|> do return nm
                   -}
                   return nm

parseSust :: Env -> Expr -> TParser b Expr
parseSust env e0 = 
         do cntParens ("[") ("]") p
       where p = do vs <- parseLocalVarList 
                    acceptTk (Token SYMBOL ":=")
                    es <- parseExprList env
                    when (length vs /= length es) 
                          (fail $ "Different lengths in substitution")
                    return (Subs e0 (zip vs es))

parseContainer :: Env -> String -> TParser b (Expr)
parseContainer env openS = 
    do d <- getDictionary
       case getOperators d [Part openS] of
            [] -> fail $ "Invalid container begining with '" ++ openS ++"'"
            (Closed mx@[Part open,Place,Part close] s:_) -> cntParens open close (p mx)
       where p name = do try (do (vs,r,e) <- parseBind env
                                 return $ (Comp name vs r e))
                     <|> do es <- parseExprList env
                            return $ (Ext name es)

true :: String
true = "True"

false :: String
false = "False"

-- ("true",tsBool)
eTrue :: a -> Assump a
eTrue x = (true,x)

eFalse :: a -> Assump a
eFalse x = (false,x)

parseRange :: Env -> TParser b (Expr)
parseRange env = option cstTrue (try $ parseExpr' env)

parseBody :: Env -> TParser b (Expr)
parseBody env = parseExpr' env

parseOptionalBody :: Env -> (Expr) -> TParser b (Expr)
parseOptionalBody env v = do acceptTk colon
                             parseBody env 
                         <|> return v 

parseTuple :: Env -> TParser b (Expr)
parseTuple env = do es <- parseExprList env
                    if length es == 1
                    then return $ Paren (head es)
                    else do return $ (Tup es)

parseExprList :: Env -> TParser b [Expr]
parseExprList env = sepBy (parseExpr' env)  (acceptTk comma)

runParse :: Dictionary b -> ([Identifier] -> TParser b a) -> String -> Either ParseError a
runParse state0 p s = runP (p envIni) state0 "(unknown)" (map feither $ tokenizer "unknown" s)
     where feither = either (error . show) id
           envIni = assumpNames $ var state0 ++ mvar state0 ++ constant state0

runp :: Dictionary b -> TParser b a -> SourceName -> String     
            -> Either ParseError (a,[TokenPos],SourcePos)
runp state0 p sn s = runIdentity $ runPT' p state0 sn (map feither $ tokenizer sn s)
            where feither = either (error . show) id

runPT' :: Monad m => ParsecT t1 u m t -> u -> SourceName -> t1 -> 
                             m (Either ParseError (t, t1, SourcePos))
runPT' p u name s
    = do res <- runParsecT p (State s (initialPos name) u)
         r <- parserReply res
         case r of
           Ok x (State inp pos _) _  -> return (Right (x,inp,pos))
           Error err -> return (Left err)
    where
        parserReply res
            = case res of
                Consumed r -> r
                Empty    r -> r
