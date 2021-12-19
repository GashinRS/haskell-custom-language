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

data Statement = VarStm VariableExp
                | IfStm IfExp
                | PrintStm PrintExp
                | WhileStm WhileExp
                | FunctionDeclStm FunctionDeclExp
                | FunctionCallStm FunctionCallExp
                deriving(Show)

parseStatement :: Parser [Statement]
parseStatement =  parseVariableExp <|> parseIfExp <|> parseWhileExp <|> parsePrintExp <|> parseFunctionDeclExp <|> parseFunctionCallExp

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

parseArguments :: Parser [IntExp]
parseArguments = parseArgBegin <|> parseArgEnd <|> parseNoArg
    where parseArgBegin = do s <- parseIntExp
                             token ','
                             ss <- parseArguments
                             return $ s:ss
          parseArgEnd   = do s <- parseIntExp
                             token ')'
                             return [s]
          parseNoArg    = do token ')'
                             return []

parseStringArguments :: Parser [String]
parseStringArguments = parseArgBegin <|> parseArgEnd <|> parseNoArg
    where parseArgBegin = do s <- parseString
                             token ','
                             ss <- parseStringArguments
                             return $ s:ss
          parseArgEnd   = do s <- parseString
                             token ')'
                             return [s]
          parseNoArg    = do token ')'
                             return []

data FunctionDeclExp = FunctionDecl String [String] [Statement] IntExp deriving(Show)

data FunctionCallExp = FunctionCall String [IntExp] deriving(Show)

parseFunctionDeclExp :: Parser [Statement]
parseFunctionDeclExp = parseDeclarationBegin <|> parseDeclarationEnd <|> parseOnlyReturnBegin <|> parseOnlyReturnEnd
    where parseDeclarationBegin = do match "function"
                                     name <- parseString
                                     token '('
                                     arguments <- parseStringArguments
                                     token '{'
                                     statements <- parseStatement
                                     match "return"
                                     returnValue <- parseIntExp
                                     match ";}"
                                     remainder <- parseStatement
                                     return $ FunctionDeclStm (FunctionDecl name arguments statements returnValue):remainder
          parseDeclarationEnd   = do match "function"
                                     name <- parseString
                                     token '('
                                     arguments <- parseStringArguments
                                     token '{'
                                     statements <- parseStatement
                                     match "return"
                                     returnValue <- parseIntExp
                                     match ";}"
                                     return [FunctionDeclStm $ FunctionDecl name arguments statements returnValue]
          parseOnlyReturnBegin  = do match "function"
                                     name <- parseString
                                     token '('
                                     arguments <- parseStringArguments
                                     token '{'
                                     match "return"
                                     returnValue <- parseIntExp
                                     match ";}"
                                     remainder <- parseStatement
                                     return $ FunctionDeclStm (FunctionDecl name arguments [] returnValue):remainder
          parseOnlyReturnEnd    = do match "function"
                                     name <- parseString
                                     token '('
                                     arguments <- parseStringArguments
                                     token '{'
                                     match "return"
                                     returnValue <- parseIntExp
                                     match ";}"
                                     return [FunctionDeclStm $ FunctionDecl name arguments [] returnValue]  

parseFunctionCallExp :: Parser [Statement]
parseFunctionCallExp = parseCallBegin <|> parseCallEnd
    where
    parseCallBegin = do match "call"
                        name <- parseString
                        token '('
                        arguments <- parseArguments
                        token ';'
                        remainder <- parseStatement
                        return $ FunctionCallStm (FunctionCall name arguments):remainder
    parseCallEnd   = do match "call"
                        name <- parseString
                        token '('
                        arguments <- parseArguments
                        token ';'
                        return [FunctionCallStm (FunctionCall name arguments)]

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
         | FunctionCallLit String [IntExp]
         | IntExp :+: IntExp
         | IntExp :*: IntExp
         | IntExp :-: IntExp
         | IntExp :/: IntExp
         deriving (Eq,Show)

parseIntExp :: Parser IntExp
parseIntExp = parseIntLit <|> parseVarLit <|> parseFunctionCallLit <|> parseAdd <|> parseSub <|> parseMul <|> parseDiv
    where
    parseIntLit = do IntLit <$> parseInt
    parseVarLit = do VarLit <$> parseString
    parseFunctionCallLit = do match "call"
                              name <- parseString
                              token '('
                              FunctionCallLit name <$> parseArguments
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