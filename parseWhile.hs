module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Boolean expressions
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)
-- Boolean binary operation (and, or)
data BBinOp = And | Or deriving (Show)  
-- Relation binary operation <, > on arithmetic expressions
data RBinOp = Greater | Less deriving (Show)

-- Arithmetic Expressions
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           -- arith expression / arith expression
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide 
            deriving (Show)

-- Statements
data Stmt = Seq [Stmt]
         | Assign String AExpr
         | If BExpr Stmt Stmt
         | While BExpr Stmt
         | Skip
         deriving (Show)

-- Lexer

languageDef = 
    emptyDef {
        -- Use C style comments
        Token.commentStart = "/*",
        Token.commentEnd   = "*/",
        Token.commentLine  = "//",
        -- Force idetifiers to start with a letter,
        -- followed by alpha numberic characters.
        Token.identStart   = letter,
        Token.identLetter  = alphaNum,
        -- Reserve identifier names and ops
        Token.reservedNames = [
            "if",
            "then",
            "else",
            "while",
            "do",
            "skip",
            "true",
            "false",
            "not",
            "and",
            "or"
        ],
        Token.reservedOpNames = [
            "+", "-", "*", "/", ":=", "<",
            ">", "and", "or", "not"
        ]
    }

-- Create lexer based on the above definition
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-- A program in this language is simply a statement so 
-- the main parser basically parses a statement. 
-- We need to remove whitespace from the beginning.

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

-- parse a group of statements between parenthesis or a list of statements.
statement :: Parser Stmt
statement = parens statement <|> sequenceOfStmt

-- parse sequence of statements separated by semicolons
sequenceOfStmt = do
    list <- (sepBy1 statement' semi)
    -- if there is only one statement, return it without using sequence
    return $ if length list == 1 then head list else Seq list

-- parse an actual singular statement
statement' :: Parser Stmt
statement' = ifStmt
         <|> whileStmt
         <|> skipStmt
         <|> assignStmt

-- let's now define the parsers for all possible statements by using 
-- the parsers from the lexers and creating the data structures

ifStmt :: Parser Stmt
ifStmt = do
    reserved "if"
    cond <- bExpression
    reserved "then"
    stmt1 <- statement
    reserved "else"
    stmt2 <- statement
    return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
    reserved "while"
    cond <- bExpression
    reserved "do"
    stmt <- statement
    return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    expr <- aExpression
    return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

-- Expressions 

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [
              [Prefix (reservedOp "-" >> return (Neg))],
              [Infix (reservedOp "*" >> return (ABinary Multiply)) AssocLeft],
              [Infix (reservedOp "/" >> return (ABinary Divide)) AssocLeft],
              [Infix (reservedOp "+" >> return (ABinary Add)) AssocLeft],
              [Infix (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]
            ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not             ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)

-- Handy function for testing the parser

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r












