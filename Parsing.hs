module Parsing(runBParser) where

import Control.Applicative((<*),(*>))
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr
import Data.List(nub)

import BTree


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
        -- the opLetter should be something like oneOf $ nub $ concat $ map tail $ allOp
        -- but that its not strict enought.
        -- therefore I do not use this mecanism from Parsec
        , opLetter = undefined
        , reservedOpNames = allOp
        , reservedNames = allKw
        }
        
TokenParser
  { parens = m_parens
  , identifier = m_identifier
  , reserved = m_reserved
  , integer = m_integer
  , whiteSpace = m_whiteSpace
  , lexeme = m_lexeme
  } = makeTokenParser def
  
  
-- I do not use the vanilla Parsec reservedOp because there
-- are too many operators that are prefix of other operators.
-- instead my m_reservedOp only accept operators from the 
-- list allOp.

m_reservedOp name =
  m_lexeme $ try $
  do{ string name
    ; notFollowedBy (possibleLongerOpe name) <?> ("end of " ++ show name)
    }
        
        
-- This thing is probably not very fast.
-- I think it should be possible to do some pre-computation.

possibleLongerOpe name = oneOf (nub starting)
  where
    starting = map (!! (length name)) possibles
    possibles = filter (\x -> take (length name) x == name) longEnough
    longEnough = filter (\x -> length name < length x) allOp
    
----------------------------------------------------------------

-- I need a particular state to resolve some ambiguities of the
-- B language:
--  * I use acceptCommaPair to discriminate between the comma used
--    in the list of expressions and the comma used in the pairs.
--    The rule I use is: the pair inside a expression list must
--    be surrounded by parenthesis.

type ParsingType = Parsec String ParsingState

data ParsingState = ParsingState
  { acceptCommaPair :: Bool
  } deriving (Show)

startParsingState = ParsingState { acceptCommaPair = True }

runBParser :: SourceName -> String -> Either ParseError BComponent
runBParser = runParser readBFile startParsingState

----------------------------------------------------------------

readBFile = m_whiteSpace >> readComponent <* eof
        
readIdent = do
  ident <- m_identifier
  return $ BIdent ident
        
readIdentList =
  readIdent `sepBy1` m_reservedOp ","
        
readComponent :: ParsingType BComponent
readComponent = do
  componentType <- m_reserved "MACHINE" *>  return BMachine <|>
                   m_reserved "REFINEMENT" *>  return BRefinement <|>
                   m_reserved "IMPLEMENTATION" *>  return BImplementation
  name <- readIdent
  clauses <- readClauses
  m_reserved "END"
  return $ BComponent componentType name clauses
  
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
  readOperation `sepBy1` (m_reservedOp ";;") --TODO change me later

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
      
----------------------------------------------------------------  

readSub = (
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
  try readSubBecomeSuchThat <|>
  try readSubOpeCall )
    `chainl1` (
            m_reservedOp ";"  *> return (BSubstitutionCompo BOpSubSeq)
        <|> m_reservedOp "||" *> return (BSubstitutionCompo BOpSubParal) )

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
  (kind,kw,kw') <- m_reserved "ANY" *> return (BAny, "WHERE", "THEN") <|>
                   m_reserved "LET" *> return (BLet, "BE", "IN")
  vars <- readIdentList
  m_reserved kw
  p <- readPredicate
  m_reserved kw'
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

readSubOpeCall = do
  outs <- option [] (try (readIdentList <* m_reservedOp "<--"))
  name <- readIdent
  ins <- option [] (m_parens readExprList)
  return $ BSubstitutionOpeCall outs name ins
  
----------------------------------------------------------------  
  
readIdentListProtected =
  do { i <- readIdent; return [i] } <|>
  m_parens (readIdent `sepBy` m_reservedOp ",")
  
readPredicateList =
  readPredicate `sepBy1` m_reservedOp ";"

readPredicate =
  buildExpressionParser predTable readPredTerm <?> "predicate"
    
readQuantPred = do
  kind <- m_reservedOp "!" *> return BUniversal <|>
          m_reservedOp "#" *> return BExistential
  vars <- readIdentListProtected
  m_reservedOp "."
  p <- m_parens readPredicate
  return $ BQuantifiedPredicate kind vars p

predTable =
  [  [Prefix (m_reserved "not" >> (return $ BUnaryPredicate BNegation))]
  ,  [Infix (m_reservedOp "=>" >> (return $ BBinaryPredicate BImplication) ) AssocLeft
     ,Infix (m_reservedOp "<=>" >> (return $ BBinaryPredicate BEquivalence) ) AssocLeft]
  ,  [Infix (m_reservedOp "&" >> (return $ BBinaryPredicate BConjunction) ) AssocLeft
     ,Infix (m_reserved "or" >> (return $ BBinaryPredicate BDisjunction) ) AssocLeft]
  ]
  
readPredTerm =
  try (m_parens readPredicate) <|>
  readQuantPred <|>
  readComp
  
readComp = do
  left <- readExpr
  kind <- readOpe
  right <- readExpr
  return $ BComparisonPredicate kind left right
  where
    readOpe =
      m_reservedOp "=" *> return BEquality <|>
      m_reservedOp "/=" *> return BNonEquality <|>
      m_reservedOp ":" *> return BMembership <|>
      m_reservedOp "/:" *> return BNonMembership <|>
      m_reservedOp "<:" *> return BInclusion <|>
      m_reservedOp "<<:" *> return BStrictInclusion <|>
      m_reservedOp "/<:" *> return BNonInclusion <|>
      m_reservedOp "/<<:" *> return BNonStrictInclusion <|>
      m_reservedOp "<=" *> return BInequality <|>
      m_reservedOp "<" *> return BStrictInequality <|>
      m_reservedOp ">=" *> return BReverseInequality <|>
      m_reservedOp ">" *> return BStrictReverseInequality
  
----------------------------------------------------------------  


readExprList = do
  s <- getState
  let previous = acceptCommaPair s
  updateState $ \s -> s { acceptCommaPair = False }
  es <- readExpr `sepBy1` m_reservedOp ","
  updateState $ \s -> s { acceptCommaPair = previous }
  return es

chainl1WithTail p op = do { x <- p; rest x }
  where
    rest x = do { (f,q) <- op
                ; y <- p
                ; res <- rest (f x y)
                ; q
                ; return res
                }
              <|> return x
              
opApply = do
  s <- getState
  let previous = acceptCommaPair s
  (kind,tailOp) <- m_reservedOp "(" *> return (BApplication, tailParen ")" previous) <|>
                   m_reservedOp "[" *> return (BImage, tailParen "]" previous)
  updateState $ \s -> s { acceptCommaPair = True }
  return (BBinaryExpression kind, tailOp)
  where
    tailParen op prev = do
      m_reservedOp op
      updateState $ \s -> s { acceptCommaPair = prev }
  

-- This one fail to accept comma pair inside application argument ("f(x,x)")
-- but give a high priority to the application ("2+f(x)" gives "2+(f(x))")
{-
readExpr = buildExpressionParser exprTable termAndCall <?> "expression"
  where termAndCall = chainl1WithTail exprTerm opApply
-}


-- This one accept comma pair inside application argument ("f(x,x)")
-- but fail on the priority of the application ("2+f(x)" gives "(2+f)(x)")
readExpr = chainl1WithTail xx opApply
  where
    xx = buildExpressionParser exprTable exprTerm <?> "expression"

-- without application at all
--readExpr = buildExpressionParser exprTable exprTerm <?> "expression"

exprTable =
  [
  -- ???:
    [Prefix (m_reservedOp "-" *> return (BUnaryExpression BOpposite))
    ,Postfix (m_reservedOp "~" *> return (BUnaryExpression BInverse))]
  -- 200:
  , [Infix (m_reservedOp "**" *> return (BBinaryExpression BPower)) AssocRight]
  -- 190:
  , [Infix (m_reservedOp "*" *> return (BBinaryExpression BAsterisk)) AssocLeft
    ,Infix (m_reservedOp "/" *> return (BBinaryExpression BDivision)) AssocLeft
    ,Infix (m_reserved "mod" *> return (BBinaryExpression BModulo)) AssocLeft]
  -- 180:
  , [Infix (m_reservedOp "+" *> return (BBinaryExpression BAddition)) AssocLeft
    ,Infix (m_reservedOp "-" *> return (BBinaryExpression BSubstration)) AssocLeft]
  -- 170:
  , [Infix (m_reservedOp ".." *> return (BBinaryExpression BInterval)) AssocLeft]
  -- 160:
  , [Infix (m_reservedOp "|->" *> return (BPair BMapsToPair)) AssocLeft
    ,Infix (m_reservedOp "\\/" *> return (BBinaryExpression BUnion)) AssocLeft
    ,Infix (m_reservedOp "/\\" *> return (BBinaryExpression BIntersection)) AssocLeft
    ,Infix (m_reservedOp "><" *> return (BBinaryExpression BDirectProduct)) AssocLeft
    ,Infix (m_reservedOp "<|" *> return (BBinaryExpression BDomainRestriction)) AssocLeft
    ,Infix (m_reservedOp "<<|" *> return (BBinaryExpression BDomainSubstraction)) AssocLeft
    ,Infix (m_reservedOp "|>" *> return (BBinaryExpression BRangeRestriction)) AssocLeft
    ,Infix (m_reservedOp "|>>" *> return (BBinaryExpression BRangeSubstraction)) AssocLeft
    ,Infix (m_reservedOp "<+" *> return (BBinaryExpression BOverloading)) AssocLeft
    ,Infix (m_reservedOp "^" *> return (BBinaryExpression BConcatenation)) AssocLeft
    ,Infix (m_reservedOp "->" *> return (BBinaryExpression BHeadInsertion)) AssocLeft
    ,Infix (m_reservedOp "<-" *> return (BBinaryExpression BTailInsertion)) AssocLeft
    ,Infix (m_reservedOp "/|\\" *> return (BBinaryExpression BHeadRestriction)) AssocLeft
    ,Infix (m_reservedOp "\\|/" *> return (BBinaryExpression BTailRestriction)) AssocLeft]
  -- 125:
  , [Infix (m_reservedOp "<->" *> return (BBinaryExpression BRelation)) AssocLeft
    ,Infix (m_reservedOp "+->" *> return (BBinaryExpression BPartialFunction)) AssocLeft
    ,Infix (m_reservedOp "-->" *> return (BBinaryExpression BTotalFunction)) AssocLeft
    ,Infix (m_reservedOp ">+>" *> return (BBinaryExpression BPartialInjection)) AssocLeft
    ,Infix (m_reservedOp ">->" *> return (BBinaryExpression BTotalInjection)) AssocLeft
    ,Infix (m_reservedOp "+->>" *> return (BBinaryExpression BPartialSurjection)) AssocLeft
    ,Infix (m_reservedOp "-->>" *> return (BBinaryExpression BTotalSurjection)) AssocLeft
    ,Infix (m_reservedOp ">->>" *> return (BBinaryExpression BTotalBijection)) AssocLeft]
  -- 115:
  , [Infix readCommaPair AssocLeft]
  -- 20:
  , [Infix (m_reservedOp ";;;" *> return (BBinaryExpression BComposition)) AssocLeft --TODO remove me
    ,Infix (m_reservedOp "|||" *> return (BBinaryExpression BParallelProduct)) AssocLeft --TODO remove me
    ]
  ] where
    readCommaPair = do
      s <- getState
      if acceptCommaPair s
      then do
        m_reservedOp "," *> return (BPair BCommaPair)
      else do
        -- this message will never be seen (normally) because all failures
        -- will be backtracked by a Parsec try functor somewhere
        parserFail "Pairs with comma are forbidden here."

exprTerm =
  readValueIdent <|>
  readParenExpr <|>
  readNumber <|>
  readSpecialIdent <|>
  readBoolConv <|>
  readBuiltinCall <|>
  readBuiltinCallCouple <|>
  readQuantExpr <|>
  readSetExpr <|>
  readListExpr
  where
    readParenExpr = do
      s <- getState
      let previous = acceptCommaPair s
      updateState $ \s -> s { acceptCommaPair = True }
      e <- m_parens readExpr
      updateState $ \s -> s { acceptCommaPair = previous }
      return e
  
readValueIdent = do
  x <- readIdent
  suffix <- option BCurrent (m_reservedOp "$0" *> return BPrevious)
  return $ BIdentifier x suffix

readNumber = do
  n <- m_integer
  return $ BNumber n
  
readBoolConv = do
  m_reserved "bool"
  p <- m_parens readPredicate
  return $ BBoolConversion p
  
readSpecialIdent =
  specialKeyword "TRUE" <|>
  specialKeyword "FALSE" <|>
  specialKeyword "MAXINT" <|>
  specialKeyword "MININT" <|>
  specialKeyword "INTEGER" <|>
  specialKeyword "NATURAL" <|>
  specialKeyword "NATURAL1" <|>
  specialKeyword "NAT" <|>
  specialKeyword "NAT1" <|>
  specialKeyword "INT" <|>
  specialKeyword "BOOL"
  where
    specialKeyword keyword =
      m_reserved keyword *> return (BIdentifier (BIdent keyword) BCurrent)

readBuiltinCall = do
  kind <- ope
  e <- m_parens readExpr
  return $ BUnaryExpression kind e
  where
    ope =
      m_reserved "max" *> return BMaximum <|>
      m_reserved "min" *> return BMinimum <|>
      m_reserved "card" *> return BCardinality <|>
      m_reserved "POW" *> return BPowerSet <|>
      m_reserved "POW1" *> return BNonEmptyPowerSet <|>
      m_reserved "FIN" *> return BFinitePowerSet <|>
      m_reserved "FIN1" *> return BNonEmptyFinitePowerSet <|>
      m_reserved "union" *> return BGeneralizedUnion <|>
      m_reserved "inter" *> return BGeneralizedIntersection <|>
      m_reserved "id" *> return BIdentity <|>
      m_reserved "closure" *> return BClosure <|>
      m_reserved "closure1" *> return BNonReflexiveClosure <|>
      m_reserved "dom" *> return BDomain <|>
      m_reserved "ran" *> return BRange <|>
      m_reserved "fnc" *> return BFunctionTransformation <|>
      m_reserved "rel" *> return BRelationTransformation <|>
      m_reserved "seq" *> return BSequence <|>
      m_reserved "seq1" *> return BNonEmptySequence <|>
      m_reserved "iseq" *> return BInjectiveSequence <|>
      m_reserved "iseq1" *> return BNonEmptyInjectiveSequence <|>
      m_reserved "perm" *> return BPermutation <|>
      m_reserved "size" *> return BSize <|>
      m_reserved "first" *> return BFirst <|>
      m_reserved "last" *> return BLast <|>
      m_reserved "front" *> return BFront <|>
      m_reserved "tail" *> return BTail <|>
      m_reserved "rev" *> return BRev <|>
      m_reserved "conc" *> return BGeneralizedConcatenation
      
readBuiltinCallCouple = do
  kind <- m_reserved "prj1" *> return BLeftProjection <|>
          m_reserved "prj2" *> return BRightProjection <|>
          m_reserved "iterate" *> return BIteration
  s <- getState
  let previous = acceptCommaPair s
  updateState $ \s -> s { acceptCommaPair = False }
  m_reservedOp "("
  e <- readExpr
  m_reservedOp ","
  f <- readExpr
  m_reservedOp ")"
  updateState $ \s -> s { acceptCommaPair = previous }
  return $ BBinaryExpression kind e f

readQuantExpr = do
  kind <- m_reserved "SIGMA" *> return BSum <|>
          m_reserved "PI" *> return BProduct <|>
          m_reserved "UNION" *> return BQuantifiedUnion <|>
          m_reserved "INTER" *> return BQuantifiedIntersection <|>
          m_reservedOp "%" *> return BLambdaExpression
  xs <- readIdentListProtected
  m_reservedOp "."
  m_reservedOp "("
  p <- readPredicate
  m_reservedOp "|"
  e <- readExpr
  m_reservedOp ")"
  return $ BQuantifiedExpression kind xs p e

readSetExpr = do
  do m_reservedOp "{"
     xs <- readIdentList
     m_reservedOp "|"
     p <- readPredicate
     m_reservedOp "}"
     return $ BSetComprehension xs p
  <|>
  do m_reservedOp "{{" -- TODO remove me
     es <- readExprList
     m_reservedOp "}}"
     return $ BSetExtension es
  
readListExpr = do
  m_reservedOp "["
  xs <- readExprList
  m_reservedOp "]"
  return $ BSequenceExtension xs --TODO factorize BTree with BSetExtension

  
----------------------------------------------------------------

allKw = nub $ kwClauses ++ kwSubst ++ kwPred ++ kwExpr
allOp = nub $ opVarious ++ opSubst ++ opPred ++ opComp ++ opExpr

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
  , ";;" -- TODO remove me
  , ";;;" -- TODO remove me
  , "|||" -- TODO remove me
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
  , ":("  -- TODO split this lexem in two ?
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
  , "{"
  , "}"
  , "{{" --TODO remove me
  , "}}" --TODO remove me
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
 
  
