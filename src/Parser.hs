module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intseq :: Parser (Exp Int)
intseq =  chainl1 intass (do {reservedOp lis ","; return ESeq})

intass :: Parser (Exp Int)
intass = try (do {
              v <- identifier lis;
                  reservedOp lis "=";
                  e <- intass;
                  return (EAssign v e)})
              <|> (do {intexp})
  
intexp :: Parser (Exp Int)
intexp  = chainl1 intterm plusOp

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor timesOp

plusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusOp = try (do{reservedOp lis "+"; return (Plus)}) 
          <|> (do{reservedOp lis "-"; return (Minus)})

timesOp :: Parser (Exp Int -> Exp Int -> Exp Int)
timesOp = try (do{reservedOp lis "*" ; return (Times)})
         <|> (do{reservedOp lis "/" ; return (Div)})

intfactor :: Parser (Exp Int)
intfactor = try (do{reservedOp lis "-"; x <- intfactor ; return (UMinus x)})
             <|> try (do {n <- natural lis; return (Const (fromIntegral n))}) 
              <|> try (do {n <- identifier lis; return (Var n)})
                <|> (do {n <- parens lis intseq; return (n)})


-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 booland (do {reservedOp lis "||"; return (Or)})

booland :: Parser (Exp Bool)
booland  = chainl1 boolnot (do {reservedOp lis "&&"; return (And)})

boolnot :: Parser (Exp Bool)
boolnot = try (do {reservedOp lis "!"; n <- boolterm; return (Not n)})
            <|> (do {n <- boolterm; return n})

boolterm :: Parser (Exp Bool)
boolterm  = try (do {reserved lis "true"; return BTrue})
              <|> (do {reserved lis "false"; return BFalse})
                <|> (do {n <- parens lis boolexp; return (n)})
                  <|> (do {exp1 <- intseq;
                      op <- (try (do {reservedOp lis "=="; return Eq})
                              <|> try (do {reservedOp lis "!="; return NEq})
                                <|> try (do {reservedOp lis "<"; return Lt})
                                  <|> try (do {reservedOp lis ">"; return Gt}));
                      exp2 <- intseq;
                      return (op exp1 exp2)})

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm  = chainl1 commin (do {reservedOp lis ";"; return Seq})

commin :: Parser Comm
commin =  try (do {reserved lis "skip" ; return Skip})
          <|> try (do {v <- identifier lis ;reservedOp lis "="; e <- intseq;
                return (Let v (e))})
            <|> try (do {reserved lis "if" ; be <- boolexp ; ct <- braces lis comm;
                reserved lis "else"; cf <- braces lis comm;
                return (IfThenElse (be) ct cf)}) --if else 
              <|> try (do {reserved lis "if" ; be <- boolexp; ct <- braces lis comm;
                  return (IfThenElse (be) ct Skip) }) --if
                <|> (do {reserved lis "while"; be <- boolexp; cw <- braces lis comm;
                      return (While (be) cw)}) --while

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
