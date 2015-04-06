module MixFix.Lexer
    ( Token(..)
    , LexicalCategory (..)
    , TokenPos
    , tokenize
    , tokenizer
    , isTkSymbol
    , isTkName
    , isTkOpen
    , isTkClose
    , isTkNumber
    ) where

import Text.ParserCombinators.Parsec hiding (token, tokens)
import Control.Applicative ((<*), (*>), (<$>), (<*>), (<$))
import qualified Text.Parsec.Prim as N
import Text.Parsec.Pos
import Control.Monad.Identity
import Data.Char

-- | Lexical Category
data LexicalCategory = LEFT_PAREN
                     | RIGHT_PAREN
                     | OPEN
                     | CLOSE
                     | SYMBOL
                     | NAME
                     | NUMBER
                     | EOF
                     deriving (Show, Eq)

-- | The token data
data Token = Token { lexCategory :: LexicalCategory, lexValue :: String } 
           deriving (Show,Eq)

-- | Token with position
type TokenPos = (Token, SourcePos)

-- | True if the token is in the symbol lexical category
isTkSymbol :: Token -> Bool
isTkSymbol tk = lexCategory tk == SYMBOL

isTkOpen :: Token -> Bool
isTkOpen tk = lexCategory tk == OPEN 

isTkClose :: Token -> Bool
isTkClose tk = lexCategory tk == CLOSE

isTkName :: Token -> Bool
isTkName tk = lexCategory tk == NAME 

isTkNumber :: Token -> Bool
isTkNumber tk = lexCategory tk == NUMBER

isCategory :: GeneralCategory -> Char -> Bool
isCategory cat c = generalCategory c == cat

open :: Parser Token
open = Token OPEN . return <$> satisfy (isCategory OpenPunctuation)

close :: Parser Token
close = Token CLOSE . return <$> satisfy (isCategory ClosePunctuation)

symbol :: Parser Token
symbol = Token SYMBOL <$>
         many1 (satisfy (\c -> isSymbol c || isCategory OtherPunctuation c
                                          || isCategory DashPunctuation c))

number :: Parser Token
number = Token NUMBER <$> ((++) <$> many1 digit <*>
                           option "" ((:) <$> char '.' <*> many1 digit)) 

name :: Parser Token
name = Token NAME <$> ((:) <$> letter <*> many alphaNum)

reserved :: Char -> LexicalCategory -> Parser Token
reserved s tk = Token tk . return <$> char s 

punctuator = [ 
           ('(',LEFT_PAREN)
           ,(')',RIGHT_PAREN)
           ]

end :: Parser Token
end = Token EOF "" <$ eof

token :: Parser Token
token = choice $ [ reserved c tk | (c,tk) <- punctuator ]
                 ++[name,number,open,close,symbol]

parsePos :: Parser Token -> Parser TokenPos
parsePos p = flip (,) <$> getPosition <*> p

tokens :: Parser [TokenPos]
tokens = spaces *> many (parsePos token <* spaces)

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser (do { xs <- tokens ; x <- parsePos end; return (xs++[x]) }) ()

tokenizer :: SourceName -> String -> [Either ParseError TokenPos]
tokenizer src str = loop initialState
  where runParser = runPT' src (spaces *> parsePos (token <|> end)) 
        initialState = (State str (initialPos src) ())
        loop st = case runIdentity $ runParser st of
                       Right (tkp@(Token EOF _,_),_) -> [Right tkp]
                       Right (x,st') -> Right x:loop st' 
                       Left  err     -> [Left err]

-- runPT' :: (Stream s m t)
--      => SourceName -> ParsecT s u m a -> u -> s -> m (Either ParseError (a,State s u))
runPT'  src p st0
    = do res <- N.runParsecT p st0
         r <- parserReply res
         case r of
           N.Ok x st _  -> return (Right (x,st))
           N.Error err -> return (Left err)
    where
        parserReply res
            = case res of
                N.Consumed r -> r
                N.Empty    r -> r
