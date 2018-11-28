module Lib where

import           Data.List
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Token
import           Text.ParserCombinators.Parsec.Combinator

newtype Type = Type String

instance Show Type where
    show (Type str) = str

newtype Identifier = Identifier String

instance Show Identifier where
    show (Identifier str) = str

data FuncDeclParam = FuncDeclParam Identifier Type

instance Show FuncDeclParam where
    show (FuncDeclParam ident typ) = show ident ++ ": " ++ show typ

data Expr = ExprIdentifier Identifier
          | ExprLiteral Integer

instance Show Expr where
    show (ExprIdentifier ident) = show ident
    show (ExprLiteral lit) = show lit

showMaybeType :: Maybe Type -> String
showMaybeType (Just t) = ": " ++ show t
showMaybeType Nothing = ""

showMaybeExpression :: Maybe Expr -> String
showMaybeExpression (Just expr) = show expr 
showMaybeExpression Nothing = ""

data Decl = DeclVar Identifier (Maybe Type) (Maybe Expr) Bool

instance Show Decl where
    show (DeclVar ident typ Nothing True) = "var " ++ show ident ++ showMaybeType typ ++ ";"
    show (DeclVar ident typ Nothing False) = "INVALID! let " ++ show ident ++ showMaybeType typ ++ ";"
    show (DeclVar ident typ (Just expr) True) = "var " ++ show ident ++ showMaybeType typ ++ " = " ++ show expr ++ ";"
    show (DeclVar ident typ (Just expr) False) = "let " ++ show ident ++ showMaybeType typ ++ " = " ++ show expr ++ ";"

newtype Return = Return Expr

instance Show Return where
    show (Return expr) = "return " ++ show expr ++ ";"

data Statement = StmtExpr Expr
               | StmtDecl Decl

instance Show Statement where
    show (StmtExpr expr) = show expr ++ ";"
    show (StmtDecl decl) = show decl

data Block = Block [Statement] Expr

instance Show Block where
    show (Block stmts expr) = "{ " ++ unwords (map show stmts) ++ " " ++ show expr ++ " }" 

data FuncDecl = FuncDecl Identifier [FuncDeclParam] Block

instance Show FuncDecl where
    show (FuncDecl ident params block) = "fn " ++ show ident ++ "(" ++ intercalate ", " (map show params) ++ ") " ++ show block

res = ["let", "var"]

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseNumber :: Parser Expr
parseNumber = ExprLiteral . read <$> many1 digit

parseAlphaNum :: Parser Char
parseAlphaNum = letter <|> digit <|> char '_'

parseIdentifier :: Parser Identifier
parseIdentifier = do
    first <- letter
    rest  <- many parseAlphaNum
    let full = first : rest
    if full `elem` res then
        unexpected ("reserved keyword: " ++ full)
    else
        return $ Identifier $ first : rest

parseType :: Parser Type
parseType = do
    first <- letter
    rest  <- many parseAlphaNum
    return $ Type $ first : rest

parseExpression :: Parser Expr
parseExpression = (ExprIdentifier <$> parseIdentifier) <|> parseNumber

validateDecl :: Decl -> Bool
validateDecl (DeclVar ident Nothing Nothing _) = False
validateDecl (DeclVar ident typ Nothing False) = False
validateDecl _ = True

parseVarDecl :: Parser Decl
parseVarDecl = do
    mutatorStr <- string "let" <|> string "var"
    ident <- spaces1 >> parseIdentifier
    typ <- optionMaybe (try $ spaces >> char ':' >> spaces >> parseType)
    expr <- optionMaybe (try $ spaces >> char '=' >> spaces >> parseExpression)
    char ';'
    let mutator = case mutatorStr of
            "let" -> False
            "var" -> True
        decl = DeclVar ident typ expr mutator
        in if validateDecl decl
            then return decl
            else fail "The decl is invalid!"

parseReturn :: Parser Return
parseReturn = Return <$> (string "return" >> spaces1 >> parseExpression <* char ';')

parseStatement :: Parser Statement
parseStatement = try $ StmtDecl <$> parseVarDecl <|>  try (StmtExpr <$> parseExpression <* char ';')

parseBlock :: Parser Block
parseBlock = between
    (char '{' >> spaces)
    (spaces >> char '}')
    (do
        stmts <- manyTill (parseStatement <* spaces)
                          (try $ lookAhead (parseExpression >> space))
        Block stmts <$> parseExpression
    )

parseFuncDeclParam :: Parser FuncDeclParam
parseFuncDeclParam = do
    ident <- parseIdentifier
    FuncDeclParam ident <$> (spaces >> char ':' >> spaces >> parseType)

parseFuncDecl :: Parser FuncDecl
parseFuncDecl = do
    ident <- string "fn" >> spaces1 >> parseIdentifier
    params <- spaces >> char '(' >> (parseFuncDeclParam `sepBy` (char ',' >> spaces)) <* char ')'
    FuncDecl ident params <$> (spaces >> parseBlock)

readParser :: Parser a -> String -> Either String a
readParser parser input = case parse parser "dusk" input of 
    Left  err -> Left $ show err
    Right val -> Right val

 -- { var x = 5; var y = 6; hello } <-- that works!
final :: String -> String
final x = (case readParser parseFuncDecl x of
            Left err -> err
            Right val -> show val) ++ "\n"
