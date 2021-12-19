{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Parser

where

import Control.Monad
import Control.Applicative
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
-- apply (Parser f) s = f s
apply (Parser f) = f

-- Return parsed value, assuming at least one successful parse
parse :: Parser a -> String -> a
parse m s = one [ x | (x,t) <- apply m s, t == "" ]
    where
    one []                 = error "no parse"
    one [x]                = x
    one xs | length xs > 1 = error "ambiguous parse"

instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    m >>= k = Parser (\s ->
                [ (y, u) |
                (x, t) <- apply m s,
                (y, u) <- apply (k x) t ])

instance MonadPlus Parser where
    mzero = Parser (const [])
    mplus m n = Parser (\s -> apply m s ++ apply n s)

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

-- Parse one character
char :: Parser Char
char = Parser f
    where
    f [] = []
    f (c:s) = [(c,s)]
-- Parse a character satisfying a predicate (e.g., isDigit)
spot :: (Char -> Bool) -> Parser Char
spot p = do { c <- char; guard (p c); return c }
-- Match a given character
token :: Char -> Parser Char
token c = spot (== c)

match :: String -> Parser String
match = mapM token

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p = plus p `mplus` return []
-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = do x <- p
            xs <- star p
            return (x:xs)

-- match a string
parseString :: Parser String
parseString = do plus $ spot isAlpha

-- parseString' :: Char -> Parser String 
-- parseString' c = do plus $ spot (/= c)

-- match a natural number
parseNat :: Parser Int
parseNat = do s <- plus (spot isDigit)
              return (read s)
-- match a negative number
parseNeg :: Parser Int
parseNeg = do token '-'
              n <- parseNat
              return (-n)
-- match an integer
parseInt :: Parser Int
parseInt = parseNat `mplus` parseNeg

parseArguments :: Parser [IntExp]
parseArguments = parseArgBegin <|> parseArgEnd
    where parseArgBegin = do s <- parseIntExp
                             token ','
                             ss <- parseArguments
                             return $ s:ss
          parseArgEnd   = do s <- parseIntExp
                             token ')'
                             return [s]

data FunctionExp = Function String [IntExp] [Statement] deriving(Show)

parseFunctionExp :: Parser [Statement]
parseFunctionExp = parseDeclaration <|> parseCall
    where parseDeclaration = do match "function"
                                s <- parseString
                                token '('
                                arguments <- parseArguments
                                token '{'
                                statements <- parseStatement
                                token '}'
                                return [FunctionStm $ Function s arguments statements]
          parseCall        = do match "call"
                                s <- parseString
                                token '('
                                arguments <- parseArguments
                                token ';'
                                return [FunctionStm $ Function s arguments []]

data Statement = VarStm VariableExp
                | IfStm IfExp
                | PrintStm PrintExp
                | WhileStm WhileExp
                | FunctionStm FunctionExp
                deriving(Show)

parseStatement :: Parser [Statement]
parseStatement = parseVarStm <|> parseIfStm <|> parseWhileStm <|> parsePrintStm <|> parseFunctionStm
    where
    parseVarStm      = do parseVariableExp
    parseIfStm       = do parseIfExp
    parseWhileStm    = do parseWhileExp
    parsePrintStm    = do parsePrintExp
    parseFunctionStm = do parseFunctionExp 

newtype VariableExp = Var (String, IntExp) deriving (Show)

parseVariableExp :: Parser [Statement]
parseVariableExp = parseIntBegin <|> parseIntEnd 
    where
    parseIntBegin  = do s <- parseString
                        token '='
                        s' <- parseIntExp
                        token ';'
                        ss <- parseStatement
                        return $ VarStm (Var (s, s')) : ss
    parseIntEnd    = do s <- parseString
                        token '='
                        s' <- parseIntExp
                        token ';'
                        return [VarStm (Var (s, s'))]

data IfExp = If BoolExp [Statement] deriving (Show)

parseIfExp :: Parser [Statement]
parseIfExp = parseBegin <|> parseEnd
    where parseBegin = do match "if"
                          token '('
                          b <- parseBoolExp
                          token ')'
                          token '{'
                          e <- parseStatement
                          token '}'
                          es <- parseStatement
                          return $ IfStm (If b e) : es
          parseEnd   = do match "if"
                          token '('
                          b <- parseBoolExp
                          token ')'
                          token '{'
                          e <- parseStatement
                          token '}'
                          return [IfStm $ If b e]

data WhileExp = While BoolExp [Statement] deriving (Show)

parseWhileExp :: Parser [Statement]
parseWhileExp = parseBegin <|> parseEnd
    where parseBegin = do match "while"
                          token '('
                          b <- parseBoolExp
                          token ')'
                          token '{'
                          e <- parseStatement
                          token '}'
                          es <- parseStatement
                          return $ WhileStm (While b e) : es
          parseEnd   = do match "while"
                          token '('
                          b <- parseBoolExp
                          token ')'
                          token '{'
                          e <- parseStatement
                          token '}'
                          return [WhileStm $ While b e]

data PrintExp = PrintInt IntExp 
            | PrintVar String
            deriving(Show)

parsePrintExp :: Parser [Statement]
parsePrintExp = parseIntBegin <|> parseIntEnd
    where parseIntBegin  = do match "print("
                              s <- parseIntExp
                              match ");"
                              ss <- parseStatement
                              return $ PrintStm (PrintInt s) : ss
          parseIntEnd    = do match "print("
                              s <- parseIntExp
                              match ");"
                              return [PrintStm (PrintInt s)]

data BoolExp = BoolLit Bool
            | IntExp :==: IntExp
            | IntExp :/=: IntExp
            | IntExp :<: IntExp
            | IntExp :<=: IntExp
            | IntExp :>: IntExp
            | IntExp :>=: IntExp
            | BoolExp :||: BoolExp
            | BoolExp :&&: BoolExp
            deriving (Eq,Show)

parseBoolExp :: Parser BoolExp
parseBoolExp = parseTrue <|> parseFalse <|> parseEq <|> parseNE <|> parseLT <|> parseLTE <|> parseGT <|> parseGTE <|> parseAnd <|> parseOr
    where
    parseTrue  = do match "true"
                    return (BoolLit True)
    parseFalse = do match "false"
                    return (BoolLit False)
    parseEq    = do token '('
                    d <- parseIntExp
                    match "=="
                    e <- parseIntExp
                    token ')'
                    return (d :==: e)
    parseNE    = do token '('
                    d <- parseIntExp
                    match "!="
                    e <- parseIntExp
                    token ')'
                    return (d :/=: e)
    parseLT    = do token '('
                    d <- parseIntExp
                    match "<"
                    e <- parseIntExp
                    token ')'
                    return (d :<: e)
    parseLTE   = do token '('
                    d <- parseIntExp
                    match "<="
                    e <- parseIntExp
                    token ')'
                    return (d :<=: e)
    parseGT    = do token '('
                    d <- parseIntExp
                    match ">"
                    e <- parseIntExp
                    token ')'
                    return (d :>: e)
    parseGTE   = do token '('
                    d <- parseIntExp
                    match ">="
                    e <- parseIntExp
                    token ')'
                    return (d :>=: e)
    parseAnd   = do token '('
                    d <- parseBoolExp
                    match "&&"
                    e <- parseBoolExp
                    token ')'
                    return (d :&&: e)
    parseOr    = do token '('
                    d <- parseBoolExp
                    match "||"
                    e <- parseBoolExp
                    token ')'
                    return (d :||: e)

runBool :: BoolExp -> Bool 
runBool (BoolLit b) = b

data IntExp = IntLit Int
         | VarLit String
         | IntExp :+: IntExp
         | IntExp :*: IntExp
         | IntExp :-: IntExp
         | IntExp :/: IntExp
         deriving (Eq,Show)

parseIntExp :: Parser IntExp
parseIntExp = parseIntLit <|> parseVarLit <|> parseAdd <|> parseSub <|> parseMul <|> parseDiv
    where
    parseIntLit = do IntLit <$> parseInt
    parseVarLit = do VarLit <$> parseString
    parseAdd    = do token '('
                     d <- parseIntExp
                     token '+'
                     e <- parseIntExp
                     token ')'
                     return (d :+: e)
    parseSub    = do token '('
                     d <- parseIntExp
                     token '-'
                     e <- parseIntExp
                     token ')'
                     return (d :-: e)
    parseMul    = do token '('
                     d <- parseIntExp
                     token '*'
                     e <- parseIntExp
                     token ')'
                     return (d :*: e)
    parseDiv    = do token '('
                     d <- parseIntExp
                     token '/'
                     e <- parseIntExp
                     token ')'
                     return (d :/: e)