module Parsing where

import Control.Applicative((<*))
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Data.List(nub)

import BTree

kwClauses =
  [ "MACHINE"
  , "REFINEMENT"
  , "IMPLEMENTATION"
  , "REFINES"
  , "SEES"
  , "IMPORTS"
  , "PROMOTES"
  , "USES"
  , "EXTENDS"
  , "INCLUDES"
  , "CONSTRAINTS"
  , "SETS"
  , "CONSTANTS"
  , "ABSTRACT_CONSTANTS"
  , "CONCRETE_CONSTANTS"
  , "PROPERTIES"
  , "VALUES"
  , "VARIABLES"
  , "ABSTRACT_VARIABLES"
  , "CONCRETE_VARIABLES"
  , "INVARIANT"
  , "ASSERTIONS"
  , "INITIALISATION"
  , "OPERATIONS"
  , "LOCAL_OPERATIONS"
  ]
  
opVarious =
  [ "$0"
  , "="
  , ","
  , "("
  , ")"
  , ";"
  , "||"
  , "*"
  , "-"
  ]
  
kwSubst =
  [ "skip"
  , "BEGIN"
  , "END"
  , "IF"
  , "THEN"
  , "ELSE"
  , "ELSIF"
  , "PRE"
  , "ASSERT"
  , "WHEN"
  , "WHERE"
  , "SELECT"
  , "CASE"
  , "OF"
  , "EITHER"
  , "VAR"
  , "ANY"
  , "CHOICE"
  , "OR"
  , "LET"
  , "BE"
  , "IN"
  , "WHILE"
  , "DO"
  , "VARIANT"
  ]

opSubst =
  [ "<--"
  , "::"
  , ":="
  , ":("
  ]

kwPred =
  [ "not"
  , "or"
  ]

opPred =
  [ "&"
  , "!"
  , "#"
  , "=>"
  , "<=>"
  ]

opComp =
  [ "/="
  , ":"
  , "/:"
  , "<:"
  , "/<:"
  , "<<:"
  , "/<<:"
  , "<"
  , "<="
  , ">"
  , ">="
  ]

kwExpr =
  [ "STRING"
  , "BOOL"
  , "TRUE"
  , "FALSE"
  , "INTEGER"
  , "INT"
  , "NATURAL"
  , "NAT"
  , "NATURAL1"
  , "NAT1"
  , "MAXINT"
  , "MININT"
  , "bool"
  , "mod"
  , "max"
  , "min"
  , "card"
  , "SIGMA"
  , "PI"
  , "POW"
  , "POW1"
  , "FIN"
  , "FIN1"
  , "union"
  , "inter"
  , "UNION"
  , "INTER"
  , "id"
  , "prj1"
  , "prj2"
  , "iterate"
  , "closure"
  , "closure1"
  , "dom"
  , "ran"
  , "fnc"
  , "rel"
  , "seq"
  , "seq1"
  , "iseq"
  , "iseq1"
  , "perm"
  , "size"
  , "first"
  , "last"
  , "front"
  , "tail"
  , "rev"
  , "conc"
  , "succ"
  , "pred"
  , "struct"
  , "rec"
  ]
  
opExpr =
  [ "+"
  , "/"
  , "**"
  , "|->"
  , "}"
  , "|"
  , ".."
  , "\\/"
  , "/\\"
  , "<->"
  , "~"
  , "><"
  , "["
  , "]"
  , "<|"
  , "<<|"
  , "|>"
  , "|>>"
  , "<+"
  , "+->"
  , "-->"
  , ">+>"
  , ">->"
  , "+->>"
  , "-->>"
  , ">+>>"
  , ">->>"
  , "%"
  , "[]"
  , "^"
  , "->"
  , "<-"
  , "/|\\"
  , "\\|/"
  , "'"
  ]
 
  
allKw = nub $ kwClauses ++ kwSubst ++ kwPred ++ kwExpr
allOp = nub $ opVarious ++ opSubst ++ opPred ++ opComp ++ opExpr

-- TODO this generic tokenizer has problems with B keywords.
def :: LanguageDef ()
def = LanguageDef
        { commentStart = "/*"
        , commentEnd = "*/"
        , commentLine = "//"
        , nestedComments = False
        , identStart = letter
        , identLetter = alphaNum <|> char '_' <|> char '.'
                                      --TODO restrict use of . in indentLetter
        , caseSensitive = True
        , opStart = oneOf $ nub $ map head allOp
        , opLetter = oneOf $ nub $ concat $ map tail $ allOp
        , reservedOpNames = allOp
        , reservedNames = allKw
        }
        
TokenParser
  { parens = m_parens
  , identifier = m_identifier
  , reservedOp = m_reservedOp
  , reserved = m_reserved
  , semiSep1 = m_semiSep1
  , whiteSpace = m_whiteSpace } = makeTokenParser def
          
exprparser :: Parsec String () BExpression
exprparser = buildExpressionParser table term <?> "expression"

table:: [[Operator String () Identity BExpression]]
table = [ [Prefix (m_reservedOp "-" >> return (BUnaryExpression BOpposite))]
        , [Infix (m_reservedOp "+" >> return (BBinaryExpression BAddition)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (BBinaryExpression BSubstration)) AssocLeft]
        ]

term :: Parsec String () BExpression
term = m_parens exprparser
       <|> (m_reserved "TRUE" >> return (BIdentifier (BIdent "TRUE") BCurrent))
       <|> (m_reserved "FALSE" >> return (BIdentifier (BIdent "FALSE") BCurrent))


readBFile = m_whiteSpace >> readComponent <* eof
        
readMachine = do
  m_reserved "MACHINE"
  return BMachine
  
readRefinement =  do
  m_reserved "REFINEMENT"
  return BRefinement
  
readImplementation =  do
  m_reserved "IMPLEMENTATION"
  return BImplementation
  
readComponent = do
  componentType <- readMachine <|> readRefinement <|> readImplementation
  name <- m_identifier
  clauses <- readClauses
  m_reserved "END"
  return $ BComponent componentType (BIdent name) clauses
  
readClauses = return []


