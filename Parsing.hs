module Parsing where

import Control.Applicative((<*),(*>))
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
  , "{}"
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
          

readBFile = m_whiteSpace >> readComponent <* eof
        
readIdent = do
  ident <- m_identifier
  return $ BIdent ident
        
readIdentList =
  readIdent `sepBy1` m_reservedOp ","
        
readComponent = do
  componentType <- readMachine <|> readRefinement <|> readImplementation
  name <- readIdent
  clauses <- readClauses
  m_reserved "END"
  return $ BComponent componentType name clauses
  
readMachine = do
  m_reserved "MACHINE"
  return BMachine
  
readRefinement =  do
  m_reserved "REFINEMENT"
  return BRefinement
  
readImplementation =  do
  m_reserved "IMPLEMENTATION"
  return BImplementation
  
-- TODO use Text.Parsec.Perm instead of many
readClauses = many $
  readRefines <|>
  readImports <|>
  readSees <|>
  readConcreteConstants <|>
  readAbstractConstants <|>
  readConcreteVariables <|>
  readAbstractVariables <|>
  readPromotes <|>
  readSets <|>
  readProperties <|>
  readInvariant <|>
  readAssertions <|>
  readValues <|>
  readInitialisation <|>
  readOperations <|>
  readLocalOperations

readRefines = do
  m_reserved "REFINES"
  name <- readIdent
  return $ BRefines name

readImports = do
  m_reserved "IMPORTS"
  names <- readIdentList
  return $ BImports names
  
readSees = do
  m_reserved "SEES"
  names <- readIdentList
  return $ BSees names
  
readConcreteConstants = do
  m_reserved "CONSTANTS" <|> m_reserved "CONCRETE_CONSTANTS"
  names <- readIdentList
  return $ BConcreteConstants names

readAbstractConstants = do
  m_reserved "ABSTRACT_CONSTANTS"
  names <- readIdentList
  return $ BAbstractConstants names

readConcreteVariables = do
  m_reserved "CONCRETE_VARIABLES"
  names <- readIdentList
  return $ BConcreteVariables names

readAbstractVariables = do
  m_reserved "VARIABLES" <|> m_reserved "ABSTRACT_VARIABLES"
  names <- readIdentList
  return $ BAbstractVariables names

readPromotes = do
  m_reserved "PROMOTES"
  names <- readIdentList
  return $ BPromotes names

readProperties = do
  m_reserved "PROPERTIES"
  p <- readPredicate
  return $ BProperties p
  
readInvariant = do
  m_reserved "INVARIANT"
  p <- readPredicate
  return $ BInvariant p
  
readAssertions = do
  m_reserved "ASSERTIONS"
  ps <- readPredicateList
  return $ BAssertions ps
  
readValues = do
  m_reserved "VALUES"
  ps <- readPredicateList
  return $ BValues ps
  
readInitialisation = do
  m_reserved "INITIALISATION"
  sub <- readSub
  return $ BInitialisation sub
  
readOperations = do
  m_reserved "OPERATIONS"
  ops <- readOperationList
  return $ BOperations ops
  
readLocalOperations = do
  m_reserved "LOCAL_OPERATIONS"
  ops <- readOperationList
  return $ BLocalOperations ops

readSets = do
  m_reserved "SETS"
  decls <- readSetsDecl
  return $ BSetClause decls
  where
    readSetsDecl = (try enumerated <|> carrier) `sepBy1` m_reservedOp ";"
    carrier = do
      name <- readIdent
      return $ BCarrierSet name
    enumerated = do
      name <- readIdent
      m_reservedOp "="
      m_reservedOp "{"
      names <- readIdentList
      m_reservedOp "}"
      return $ BEnumeratedSet name names

readOperationList =
  readOperation `sepBy1` m_reservedOp ";;" --TODO change me later

readOperation = do
  outputs <- option [] (try readOutputs)
  name <- readIdent
  inputs <- option [] readInputs
  m_reservedOp "="
  sub <- readSub
  return $ BOperation outputs name inputs sub
  where
    readOutputs = do
      xs <- readIdentList
      m_reservedOp "<--"
      return xs
    readInputs = do
      xs <- m_parens readIdentList
      return xs

readSub =
  readSubSkip <|>
  readSubBlock <|>
  readSubPre <|>
  readSubAssert <|>
  readSubChoice <|>
  readSubIf <|>
  readSubSelect <|>
  readSubCase <|>
  readSubAnyOrLet <|>
  readSubVar <|>
  readSubWhile <|>
  try readSubSimple <|>
  try readSubBecomeIn <|>
  try readSubBecomeSuchThat

readSubSkip = do
  m_reserved "skip"
  return BSubstitutionSkip
  
readSubBlock = do
  m_reserved "BEGIN"
  sub <- readSub
  m_reserved "END"
  return $ BSubstitutionBlock sub

readSubPre = do
  m_reserved "PRE"
  p <- readPredicate
  m_reserved "THEN"
  sub <- readSub
  m_reserved "END"
  return $ BSubstitutionPreCondition p sub
  
readSubAssert = do
  m_reserved "ASSERT"
  p <- readPredicate
  m_reserved "THEN"
  sub <- readSub
  m_reserved "END"
  return $ BSubstitutionAssert p sub
  
readSubChoice = do
  m_reserved "CHOICE"
  subs <- readSub `sepBy1` (m_reserved "OR")
  m_reserved "END"
  return $ BSubstitutionChoice subs

readSubCond kind kwIf kwElsif = do
  m_reserved kwIf
  p <- readPredicate
  m_reserved "THEN"
  sub <- readSub
  elsif <- many readElseIf
  els <- option Nothing readElse
  m_reserved "END"
  return $ BSubstitutionCond kind ((p,sub):elsif) els
  where
    readElse = do
      m_reserved "ELSE"
      sub <- readSub
      return $ Just sub
    readElseIf = do
      m_reserved kwElsif
      p <- readPredicate
      m_reserved "THEN"
      sub <- readSub
      return (p,sub)
  
readSubIf = readSubCond BIf "IF" "ELSIF"
readSubSelect = readSubCond BSelect "SELECT" "WHEN"

readSubCase = do
  m_reserved "CASE"
  var <- readExpr
  m_reserved "OF"
  m_reserved "EITHER"
  e <- readExpr
  m_reserved "THEN"
  sub <- readSub
  orThen <- many readOrThen
  els <- option Nothing readElse
  m_reserved "END"
  m_reserved "END"
  return $ BSubstitutionCase var ((e,sub):orThen) els
  where
    readElse = do
      m_reserved "ELSE"
      sub <- readSub
      return $ Just sub
    readOrThen = do
      m_reserved "OR"
      e <- readExpr
      m_reserved "THEN"
      sub <- readSub
      return (e,sub)

readSubAnyOrLet = do
  kind <- (m_reserved "ANY" *> return BAny) <|>
          (m_reserved "LET" *> return BLet)
  vars <- readIdentList
  m_reserved "WHERE"
  p <- readPredicate
  m_reserved "THEN"
  sub <- readSub
  m_reserved "END"
  return $ BSubstitutionSpecVar kind vars p sub
  
readSubVar = do
  m_reserved "VAR"
  vars <- readIdentList
  m_reserved "IN"
  sub <- readSub
  m_reserved "END"
  return $ BSubstitutionVar vars sub
  
readSubWhile = do
  m_reserved "WHILE"
  p <- readPredicate
  m_reserved "DO"
  sub <- readSub
  m_reserved "INVARIANT"
  inv <- readPredicate
  m_reserved "VARIANT"
  var <- readExpr
  m_reserved "END"
  return $ BSubstitutionWhile p sub inv var

readSubSimple = do
  vars <- readIdentList
  m_reservedOp ":="
  exprs <- readExprList
  return $ BSubstitutionSimple vars exprs
  
readSubBecomeIn = do
  vars <- readIdentList
  m_reservedOp "::"
  exprs <- readExpr
  return $ BSubstitutionBecomeIn vars exprs
  
readSubBecomeSuchThat = do
  vars <- readIdentList
  m_reservedOp ":("
  p <- readPredicate
  m_reservedOp ")"
  return $ BSubstitutionSuchThat vars p

readPredicateList =
  readPredicate `sepBy1` m_reservedOp ";"

readPredicate = undefined

readExprList =
  readExpr `sepBy1` m_reservedOp ","
  
readExpr = buildExpressionParser table term <?> "expression"

table = [ [Prefix (m_reservedOp "-" >> return (BUnaryExpression BOpposite))]
        , [Infix (m_reservedOp "+" >> return (BBinaryExpression BAddition)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (BBinaryExpression BSubstration)) AssocLeft]
        ]

term = m_parens readExpr
       <|> (m_reserved "TRUE" >> return (BIdentifier (BIdent "TRUE") BCurrent))
       <|> (m_reserved "FALSE" >> return (BIdentifier (BIdent "FALSE") BCurrent))

