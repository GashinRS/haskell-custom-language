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

newtype PrintExp = Print IntExp deriving(Show)

parsePrintExp :: Parser [Statement]
parsePrintExp = parseBegin <|> parseEnd
    where parseBegin = do match "print("
                          s <- parseIntExp
                          match ");"
                          ss <- parseStatement
                          return $ PrintStm (Print s) : ss
          parseEnd   = do match "print("
                          s <- parseIntExp
                          match ");"
                          return [PrintStm (Print s)]

-- evalPrint :: PrintExp -> IO()
-- evalPrint (Print i) =  print $ evalIntExp i

data Statement = VarStm VariableExp
                | IfStm IfExp
                | PrintStm PrintExp
                | WhileStm WhileExp
                deriving(Show)

parseStatement :: Parser [Statement]
parseStatement = parseVarStm <|> parseIfStm <|> parsePrintStm
    where
    parseVarStm   = do parseVariableExp
    parseIfStm    = do parseIfExp
    parsePrintStm = do parsePrintExp

-- evalStatements :: [Statement] -> IO()
-- evalStatements [x]    = evalStatement x
-- evalStatements (x:xs) = evalStatement x  >> evalStatements xs

-- evalStatement :: Statement -> IO()
-- evalStatement (PrintStm exp) = evalPrint exp
-- evalStatement (IfStm exp)    = evalIfExp exp
-- evalStatement (VarStm exp)   = evalVarExp exp
-- evalStatement (WhileStm exp) = evalWhileExp exp

data VariableExp = IntVar (String, Int) 
                | BoolVar (String, Bool)
                deriving (Show)

-- evalVarExp :: VariableExp -> IO()
-- evalVarExp (IntVar i)  = return()
-- evalVarExp (BoolVar i) = return() 

parseVariableExp :: Parser [Statement]
parseVariableExp = parseBoolBegin <|> parseBoolEnd <|> parseIntBegin <|> parseIntEnd 
    where
    parseIntBegin  = do s <- parseString
                        token '='
                        s' <- parseInt
                        token ';'
                        ss <- parseStatement
                        return $ VarStm (IntVar (s, s')) : ss
    parseIntEnd    = do s <- parseString
                        token '='
                        s' <- parseInt
                        token ';'
                        return [VarStm (IntVar (s, s'))]
    parseBoolBegin = do s <- parseString
                        token '='
                        s' <- parseBoolExp
                        let s'' = runBool s'
                        token ';'
                        ss <- parseStatement
                        return $ VarStm (BoolVar (s, s'')) : ss
    parseBoolEnd   = do s <- parseString
                        token '='
                        s' <- parseBoolExp
                        let s'' = runBool s'
                        token ';'
                        return [VarStm (BoolVar (s, s''))]

newtype WhileExp = While BoolExp deriving (Show)

data IfExp = If BoolExp [Statement]
                deriving (Show)

-- evalIfExp :: IfExp -> IO()
-- evalIfExp (If b e)
--                 | evalBoolExp b       = evalStatements e
--                 | not $ evalBoolExp b = return()

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
-- evalBoolExp :: BoolExp -> Bool
-- evalBoolExp (BoolLit b) = b
-- evalBoolExp (e :==: f)  = evalIntExp e == evalIntExp f
-- evalBoolExp (e :/=: f)  = evalIntExp e /= evalIntExp f
-- evalBoolExp (e :<: f)   = evalIntExp e < evalIntExp f
-- evalBoolExp (e :<=: f)  = evalIntExp e <= evalIntExp f
-- evalBoolExp (e :>: f)   = evalIntExp e > evalIntExp f
-- evalBoolExp (e :>=: f)  = evalIntExp e >= evalIntExp f
-- evalBoolExp (e :&&: f)  = evalBoolExp e && evalBoolExp f
-- evalBoolExp (e :||: f)  = evalBoolExp e || evalBoolExp f

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
                    match "/="
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
         | IntExp :+: IntExp
         | IntExp :*: IntExp
         | IntExp :-: IntExp
         | IntExp :/: IntExp
         deriving (Eq,Show)
-- evalIntExp :: IntExp -> Int
-- evalIntExp (IntLit n) = n
-- evalIntExp (e :+: f)  = evalIntExp e + evalIntExp f
-- evalIntExp (e :-: f)  = evalIntExp e - evalIntExp f
-- evalIntExp (e :*: f)  = evalIntExp e * evalIntExp f
-- evalIntExp (e :/: f)  = evalIntExp e `div` evalIntExp f

parseIntExp :: Parser IntExp
parseIntExp = parseIntLit <|> parseAdd <|> parseSub <|> parseMul <|> parseDiv
    where
    parseIntLit= do IntLit <$> parseInt
    parseAdd   = do token '('
                    d <- parseIntExp
                    token '+'
                    e <- parseIntExp
                    token ')'
                    return (d :+: e)
    parseSub   = do token '('
                    d <- parseIntExp
                    token '-'
                    e <- parseIntExp
                    token ')'
                    return (d :-: e)
    parseMul   = do token '('
                    d <- parseIntExp
                    token '*'
                    e <- parseIntExp
                    token ')'
                    return (d :*: e)
    parseDiv   = do token '('
                    d <- parseIntExp
                    token '/'
                    e <- parseIntExp
                    token ')'
                    return (d :/: e)