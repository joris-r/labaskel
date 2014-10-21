--    Copyright 2014 Joris Rehm
-- 
--    Licensed under the Apache License, Version 2.0 (the "License");
--    you may not use this file except in compliance with the License.
--    You may obtain a copy of the License at
-- 
--        http://www.apache.org/licenses/LICENSE-2.0
-- 
--    Unless required by applicable law or agreed to in writing, software
--    distributed under the License is distributed on an "AS IS" BASIS,
--    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--    See the License for the specific language governing permissions and
--    limitations under the License. 

module Pretty where

import Text.PrettyPrint
import Data.List(intersperse)

import BTree

indent = nest 2

prettyBComponent :: BComponent -> Doc
prettyBComponent (BComponent componentType name clauses) =
  prettyBComponentType componentType <+> text (unBIdent name) $$
  vcat (map prettyBClause clauses) $$
  text "END"
  
prettyBComponentType :: BComponentType -> Doc
prettyBComponentType BMachine = text "MACHINE"
prettyBComponentType BRefinement = text "REFINEMENT"
prettyBComponentType BImplementation = text "IMPLEMENTATION"
  
prettyBClause :: BClause -> Doc

prettyBClause (BConcreteConstants vars) =
  text "CONSTANTS" <+> prettyVariablesList vars
  
prettyBClause (BAbstractConstants vars) =
  text "ABSTRACT_CONSTANTS" <+> prettyVariablesList vars
  
prettyBClause (BAbstractVariables vars) =
  text "VARIABLES" <+> prettyVariablesList vars
  
prettyBClause (BConcreteVariables vars) =
  text "CONCRETE_VARIABLES" <+> prettyVariablesList vars
  
prettyBClause (BPromotes vars) =
  text "PROMOTES" <+> prettyVariablesList vars
  
prettyBClause (BRefines name) =
  text "REFINES" <+> text (unBIdent name)
  
prettyBClause (BImports names) =
  text "IMPORTS" <+> prettyVariablesList names
  
prettyBClause (BSees names) =
  text "SEES" <+> prettyVariablesList names
  
prettyBClause (BSetClause decl) =
  text "SETS" <+> cat (punctuate semi (map prettyBSetDeclaration decl))
  
prettyBClause (BProperties p) =
  text "PROPERTIES" <+> prettyBPredicate p
  
prettyBClause (BInvariant p) =
  text "INVARIANT" <+> prettyBPredicate p
  
prettyBClause (BValues ps) =
  text "VALUES" <+> cat (punctuate semi (map prettyBPredicate ps))
  
prettyBClause (BAssertions ps) =
  text "ASSERTIONS" <+> cat (punctuate semi (map prettyBPredicate ps))
  
prettyBClause (BInitialisation s) =
  text "INITIALISATION" <+> prettyBSubstitution s
  
prettyBClause (BOperations ops) =
  text "OPERATIONS" $$
  indent (vcat ((punctuate (text ";")) (map prettyBOperation ops)))
  
prettyBClause (BLocalOperations ops) =
  text "LOCAL_OPERATIONS" $$
  indent (vcat ((punctuate (text ";")) (map prettyBOperation ops)))


prettyBSetDeclaration :: BSetDeclaration -> Doc
prettyBSetDeclaration (BCarrierSet name) =
  text (unBIdent name)
prettyBSetDeclaration (BEnumeratedSet name xs) =
  text (unBIdent name) <+> text "=" <+>
  braces (prettyVariablesList xs)
  
  
prettyBOperation :: BOperation -> Doc

prettyBOperation (BOperation [] name [] body) =
  text (unBIdent name) <+>
  text "=" <+> prettyBSubstitution body
  
prettyBOperation (BOperation [] name iArgs body) =
  text (unBIdent name) <+>
  parens (prettyVariablesList iArgs) <+>
  text "=" <+> prettyBSubstitution body
  
prettyBOperation (BOperation oArgs name [] body) =
  prettyVariablesList oArgs <+>
  text "<--" <+> text (unBIdent name) <+>
  text "=" <+> prettyBSubstitution body
  
prettyBOperation (BOperation oArgs name iArgs body) =
  prettyVariablesList oArgs <+>
  text "<--" <+> text (unBIdent name) <+>
  parens (prettyVariablesList iArgs) <+>
  text "=" <+> prettyBSubstitution body

  
prettyVariablesList :: [BIdent] -> Doc
prettyVariablesList =  hsep . (punctuate (text ",")) . (map $ text . unBIdent )

prettyVariablesListProtected :: [BIdent] -> Doc
prettyVariablesListProtected [var] =
  text (unBIdent var)
prettyVariablesListProtected vars =
  parens (prettyVariablesList vars)


prettyBSubstitution :: BSubstitution -> Doc

prettyBSubstitution (BSubstitutionBlock s) =
  text "BEGIN" <+> prettyBSubstitution s <+> text "END"

prettyBSubstitution BSubstitutionSkip =
  text "skip"
  
prettyBSubstitution (BSubstitutionSimple vars es) =
  prettyVariablesList vars <+>
  text ":=" <+>
  (hsep . (punctuate (text ",")) . (map prettyBExpression) $ es)
    
prettyBSubstitution (BSubstitutionBecomeIn vars e) =
  prettyVariablesList vars <+>
  text "::" <+>
  prettyBExpression e
    
prettyBSubstitution (BSubstitutionSuchThat vars p) =
  prettyVariablesList vars <+>
  text ":(" <+>
  prettyBPredicate p <+>
  text ")"
    
prettyBSubstitution (BSubstitutionPreCondition p s) =
  text "PRE" <+> prettyBPredicate p <+>
  text "THEN" <+> prettyBSubstitution s <+>
  text "END"
  
prettyBSubstitution (BSubstitutionAssert p s) =
  text "ASSERT" <+> prettyBPredicate p <+>
  text "THEN" <+> prettyBSubstitution s <+>
  text "END"
  
prettyBSubstitution (BSubstitutionChoice subs) =
  text "CHOICE" <+>
  (hsep . (intersperse $ text "OR") . (map prettyBSubstitution ) $ subs) <+>
  text "END"

prettyBSubstitution (BSubstitutionCond op ((testP,thenS):xs) elseS) =
  keyword_initial <+> prettyBPredicate testP <+>
  text "THEN" <+> prettyBSubstitution thenS <+>
  elsif xs <+>
  case elseS of
    Nothing -> empty
    (Just s) -> text "ELSE" <+> prettyBSubstitution s
  <+> text "END"
  where
    elsif :: [(BPredicate,BSubstitution)] -> Doc
    elsif ((t,s):xs) =
      keyword_other  <+> prettyBPredicate t <+>
      text "THEN" <+> prettyBSubstitution s <+>
      elsif xs
    elsif [] = empty
    keyword_initial = case op of
                           BIf -> text "IF"
                           BSelect -> text "SELECT"
    keyword_other = case op of
                           BIf -> text "ELSIF"
                           BSelect -> text "WHEN"
                           
prettyBSubstitution (BSubstitutionCase valueE ((testE,thenS):xs) elseS) =
  text "CASE" <+> parens (prettyBExpression valueE) <+> text "OF" <+>
  text "EITHER" <+> prettyBExpression testE <+>
  text "THEN" <+> prettyBSubstitution thenS <+>
  orThen xs <+>
  case elseS of
    Nothing -> empty
    (Just s) -> text "ELSE" <+> prettyBSubstitution s
  <+> text "END" <+> text "END"
  where
    orThen :: [(BExpression,BSubstitution)] -> Doc
    orThen ((t,s):xs) =
      text "OR"  <+> prettyBExpression t <+>
      text "THEN" <+> prettyBSubstitution s <+>
      orThen xs
    orThen [] = empty
    
prettyBSubstitution (BSubstitutionSpecVar op vars p s) =
  (case op of
        BAny ->  text "ANY"
        BLet ->  text "LET" ) <+>
  prettyVariablesList vars <+>
  (case op of
        BAny -> text "WHERE"
        BLet -> text "BE" ) <+>
  prettyBPredicate p <+>
  (case op of
        BAny -> text "THEN"
        BLet -> text "IN" ) <+>
  prettyBSubstitution s <+>
  text "END"

prettyBSubstitution (BSubstitutionCompo op s t) =
  prettyBSubstitution s <+>
  text (case op of
             BOpSubParal -> "||"
             BOpSubSeq -> ";")  <+>
  prettyBSubstitution t
  
prettyBSubstitution (BSubstitutionVar vars s) =
  text "VAR" <+>
  prettyVariablesList vars <+>
  text "IN" <+>
  prettyBSubstitution s <+>
  text "END"

prettyBSubstitution (BSubstitutionOpeCall [] name []) =
  text (unBIdent name)
  
prettyBSubstitution (BSubstitutionOpeCall vars name []) =
  prettyVariablesList vars <+> text "<--" <+> text (unBIdent name)
  
prettyBSubstitution (BSubstitutionOpeCall [] name exprs) =
  text (unBIdent name) <+>
  parens (hsep . (punctuate (text ",")) . (map prettyBExpression) $ exprs)
  
prettyBSubstitution (BSubstitutionOpeCall vars name exprs) =
  prettyVariablesList vars <+> text "<--" <+> text (unBIdent name) <+>
  parens (hsep . (punctuate (text ",")) . (map prettyBExpression) $ exprs)
  
prettyBSubstitution (BSubstitutionWhile condition body invariant variant) =
  text "WHILE" <+> prettyBPredicate condition <+>
  text "DO" <+> prettyBSubstitution body <+>
  text "INVARIANT" <+> prettyBPredicate invariant <+>
  text "VARIANT" <+> prettyBExpression variant <+>
  text "END"


prettyBPredicate :: BPredicate -> Doc

prettyBPredicate (BUnaryPredicate op p) =
  text "not" <+>
  parens (prettyBPredicate p)

prettyBPredicate (BBinaryPredicate op p q) =
  parens (prettyBPredicate p <+> prettyBOperatorBinPred op <+> prettyBPredicate q)
  
prettyBPredicate (BQuantifiedPredicate op vars p) =
  prettyBOperatorQuantPred op <+>
  prettyVariablesListProtected  vars<+> text "." <+>
  parens (prettyBPredicate p)
  

prettyBPredicate (BComparisonPredicate op e f) =
  parens (prettyBExpression e <+> prettyBOperatorBinPredTerm op <+> prettyBExpression f)


prettyBExpression :: BExpression -> Doc

prettyBExpression (BIdentifier s BCurrent) = text (unBIdent s)
prettyBExpression (BIdentifier s BPrevious) = text (unBIdent s) <> text "$0"

prettyBExpression (BBoolConversion p) =
  text "bool" <+> parens (prettyBPredicate p)
  
prettyBExpression (BNumber n) = integer n

prettyBExpression (BUnaryExpression BOpposite e) =
  parens (text "-" <> prettyBExpression e)
prettyBExpression (BUnaryExpression BInverse e) =
  parens( prettyBExpression e <> text "~" )
prettyBExpression (BUnaryExpression op e) =
  prettyBOperatorUnaExpr op <> parens(prettyBExpression e)
  

prettyBExpression (BBinaryExpression BLeftProjection e f) =
  text "prj1" <> parens (
    prettyBExpression e <+> text "," <+> prettyBExpression f )
prettyBExpression (BBinaryExpression BRightProjection e f) =
  text "prj2" <> parens (
    prettyBExpression e <+> text "," <+> prettyBExpression f )
prettyBExpression (BBinaryExpression BIteration e f) =
  text "iterate" <> parens (
    prettyBExpression e <+> text "," <+> prettyBExpression f )
prettyBExpression (BBinaryExpression BImage e f) =
  parens (prettyBExpression e <> brackets ( prettyBExpression f ))
prettyBExpression (BBinaryExpression BApplication e f) =
  parens (prettyBExpression e <> parens ( prettyBExpression f ))
prettyBExpression (BBinaryExpression op e f) =
  parens (prettyBExpression e <+> prettyBOperatorBinExpr op <+> prettyBExpression f)

prettyBExpression (BPair shape e f) =
  parens (prettyBExpression e <+> prettyBPairShape shape <+> prettyBExpression f)
  
prettyBExpression (BQuantifiedExpression op vars p e) =
  prettyBOperatorQuantExpr op <+>
  prettyVariablesListProtected vars <+> text "." <+>
  text "(" <+>
  prettyBPredicate p <+>
  text "|" <+>
  prettyBExpression e <+>
  text ")"

prettyBExpression (BSetComprehension vars p) = 
  text "{" <+>
  prettyVariablesList vars <+> text "|" <+>
  prettyBPredicate p  <+>
  text "}"
  
prettyBExpression (BSetExtension exprs) = 
  text "{" <+>
  (hsep . (punctuate (text ",")) . (map prettyBExpression) $ exprs )<+>
  text "}"

prettyBExpression (BSequenceExtension exprs) = 
  text "[" <+>
  (hsep . (punctuate (text ",")) . (map prettyBExpression) $ exprs )<+>
  text "]"

prettyBPairShape BCommaPair = text ","
prettyBPairShape BMapsToPair = text "|->"
  
prettyBOperatorBinPred :: BOperatorBinPred -> Doc
prettyBOperatorBinPred BConjunction = text "&"
prettyBOperatorBinPred BDisjunction = text "or"
prettyBOperatorBinPred BImplication = text "=>"
prettyBOperatorBinPred BEquivalence = text "<=>"

prettyBOperatorBinPredTerm :: BOperatorBinPredTerm -> Doc
prettyBOperatorBinPredTerm BEquality = text "="
prettyBOperatorBinPredTerm BNonEquality = text "/="
prettyBOperatorBinPredTerm BMembership = text ":"
prettyBOperatorBinPredTerm BNonMembership = text "/:"
prettyBOperatorBinPredTerm BInclusion = text "<:"
prettyBOperatorBinPredTerm BStrictInclusion = text "<<:"
prettyBOperatorBinPredTerm BNonInclusion = text "/<:"
prettyBOperatorBinPredTerm BNonStrictInclusion = text "/<<:"
prettyBOperatorBinPredTerm BInequality = text "<="
prettyBOperatorBinPredTerm BStrictInequality = text "<"
prettyBOperatorBinPredTerm BReverseInequality = text ">="
prettyBOperatorBinPredTerm BStrictReverseInequality = text ">"

prettyBOperatorQuantPred :: BOperatorQuantPred -> Doc
prettyBOperatorQuantPred BUniversal = text "!"
prettyBOperatorQuantPred BExistential = text "#"

prettyBOperatorUnaExpr :: BOperatorUnaExpr -> Doc
-- not here becase it's a special case:
--   BOpposite
--   BInverse
prettyBOperatorUnaExpr BMaximum = text "max"
prettyBOperatorUnaExpr BMinimum = text "min"
prettyBOperatorUnaExpr BCardinality = text "card"
prettyBOperatorUnaExpr BPowerSet = text "POW"
prettyBOperatorUnaExpr BNonEmptyPowerSet = text "POW1"
prettyBOperatorUnaExpr BFinitePowerSet = text "FIN"
prettyBOperatorUnaExpr BNonEmptyFinitePowerSet = text "FIN1"
prettyBOperatorUnaExpr BGeneralizedUnion = text "union"
prettyBOperatorUnaExpr BGeneralizedIntersection = text "inter"
prettyBOperatorUnaExpr BIdentity = text "id"
prettyBOperatorUnaExpr BClosure = text "closure"
prettyBOperatorUnaExpr BNonReflexiveClosure = text "closure1"
prettyBOperatorUnaExpr BDomain = text "dom"
prettyBOperatorUnaExpr BRange = text "ran"
prettyBOperatorUnaExpr BFunctionTransformation = text "fnc"
prettyBOperatorUnaExpr BRelationTransformation = text "rel"
prettyBOperatorUnaExpr BSequence = text "seq"
prettyBOperatorUnaExpr BNonEmptySequence = text "seq1"
prettyBOperatorUnaExpr BInjectiveSequence = text "iseq"
prettyBOperatorUnaExpr BNonEmptyInjectiveSequence = text "iseq1"
prettyBOperatorUnaExpr BPermutation = text "perm"
prettyBOperatorUnaExpr BSize = text "size"
prettyBOperatorUnaExpr BFirst = text "first"
prettyBOperatorUnaExpr BLast = text "last"
prettyBOperatorUnaExpr BFront = text "front"
prettyBOperatorUnaExpr BTail = text "tail"
prettyBOperatorUnaExpr BRev = text "rev"
prettyBOperatorUnaExpr BGeneralizedConcatenation = text "conc"

prettyBOperatorBinExpr :: BOperatorBinExpr -> Doc
-- not here becase it's a special case:
--   BLeftProjection
--   BRightProjection
--   BIteration
--   BImage
--   BApplication
prettyBOperatorBinExpr BAddition = text "+"
prettyBOperatorBinExpr BSubstration = text "-"
prettyBOperatorBinExpr BAsterisk = text "*"
prettyBOperatorBinExpr BDivision = text "/"
prettyBOperatorBinExpr BModulo = text "mod"
prettyBOperatorBinExpr BPower = text "**"
prettyBOperatorBinExpr BInterval = text ".."
prettyBOperatorBinExpr BUnion = text "\\/"
prettyBOperatorBinExpr BIntersection = text "/\\"
prettyBOperatorBinExpr BRelation = text "<->"
prettyBOperatorBinExpr BComposition = text ";;;"
prettyBOperatorBinExpr BDirectProduct = text "><"
prettyBOperatorBinExpr BParallelProduct = text "|||"
prettyBOperatorBinExpr BDomainRestriction = text "<|"
prettyBOperatorBinExpr BDomainSubstraction = text "<<|"
prettyBOperatorBinExpr BRangeRestriction = text "|>"
prettyBOperatorBinExpr BRangeSubstraction = text "|>>"
prettyBOperatorBinExpr BOverloading = text "<+"
prettyBOperatorBinExpr BPartialFunction = text "+->"
prettyBOperatorBinExpr BTotalFunction = text "-->"
prettyBOperatorBinExpr BPartialInjection = text ">+>"
prettyBOperatorBinExpr BTotalInjection = text ">->"
prettyBOperatorBinExpr BPartialSurjection = text "+->>"
prettyBOperatorBinExpr BTotalSurjection = text "-->>"
prettyBOperatorBinExpr BTotalBijection = text ">->>"
prettyBOperatorBinExpr BConcatenation = text "^"
prettyBOperatorBinExpr BHeadInsertion = text "->"
prettyBOperatorBinExpr BTailInsertion = text "<-"
prettyBOperatorBinExpr BHeadRestriction = text "/|\\"
prettyBOperatorBinExpr BTailRestriction = text "\\|/"

prettyBOperatorQuantExpr :: BOperatorQuantExpr -> Doc
prettyBOperatorQuantExpr BSum = text "SIGMA"
prettyBOperatorQuantExpr BProduct = text "PI"
prettyBOperatorQuantExpr BQuantifiedUnion = text "UNION"
prettyBOperatorQuantExpr BQuantifiedIntersection = text "INTER"
prettyBOperatorQuantExpr BLambdaExpression = text "%"
