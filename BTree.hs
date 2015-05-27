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

module BTree where


-- TODO use a non empty list type
-- TODO factorise clauses and substitution
-- TODO litteral such as BOOL, TRUE, INT are not in the BTree (only as a BIdentifier)

newtype BIdent = BIdent String
  deriving(Show, Read, Eq)

unBIdent :: BIdent -> String
unBIdent (BIdent a) = a

data BComponent
  = BComponent BComponentType BIdent [BClause] -- TODO use another type than [a]
  deriving(Show, Read, Eq)
  
data BComponentType
  = BMachine
  | BRefinement
  | BImplementation
  deriving(Show, Read, Eq)
  
data BClause
  = BAbstractVariables [BIdent]
  | BConcreteVariables [BIdent]
  | BConcreteConstants [BIdent]
  | BAbstractConstants [BIdent]
  | BPromotes [BIdent]
  | BRefines BIdent
  | BImports [BIdent]
  | BSees [BIdent]
  | BSetClause [BSetDeclaration]
  | BProperties BExpression
  | BInvariant BExpression
  | BAssertions [BExpression]
  | BValues [BExpression] -- TODO replace predicate by a list of (var,expr)
  | BInitialisation BSubstitution
  | BOperations [BOperation]
  | BLocalOperations [BOperation]
  deriving(Show, Read, Eq)

data BSetDeclaration
  = BCarrierSet BIdent
  | BEnumeratedSet BIdent [BIdent]
  deriving(Show, Read, Eq)
  
data BOperation
  = BOperation [BIdent] BIdent [BIdent] BSubstitution
  deriving(Show, Read, Eq)

data BSubstitution
  = BSubstitutionBlock BSubstitution
  | BSubstitutionSkip
  | BSubstitutionSimple [BIdent] [BExpression]
  
  -- TODO factorize
  | BSubstitutionPreCondition BExpression BSubstitution
  | BSubstitutionAssert       BExpression BSubstitution
  
  | BSubstitutionChoice [BSubstitution]
  | BSubstitutionCond BOperatorCond [(BExpression,BSubstitution)] (Maybe BSubstitution) -- at least one couple in the list
  | BSubstitutionCase BExpression [(BExpression,BSubstitution)] (Maybe BSubstitution) -- at least one couple in the list
  | BSubstitutionSpecVar BOperatorSpecVar [BIdent] BExpression BSubstitution
  
  -- TODO factorize
  | BSubstitutionBecomeIn [BIdent] BExpression
  | BSubstitutionSuchThat [BIdent] BExpression
  
  | BSubstitutionVar [BIdent] BSubstitution
  | BSubstitutionCompo BOperatorSubComp BSubstitution BSubstitution
  | BSubstitutionOpeCall [BIdent] BIdent [BExpression]
  | BSubstitutionWhile BExpression BSubstitution BExpression BExpression
  deriving(Show, Read, Eq)
  
data BOperatorCond = BIf | BSelect deriving(Show, Read, Eq)
data BOperatorSpecVar = BAny | BLet deriving(Show, Read, Eq)
data BOperatorSubComp = BOpSubParal | BOpSubSeq deriving(Show, Read, Eq)

data BExpression
  = BNegation                         BExpression
  | BUnaryExpression BOperatorUnaExpr BExpression
  
  | BBinaryPredicate     BOperatorBinExpr BExpression BExpression
  | BBinaryExpression    BOperatorBinExpr BExpression BExpression
  | BComparisonPredicate BOperatorBinExpr BExpression BExpression
  
  | BApply BOperatorApply BExpression BExpression
  | BBoolConversion                   BExpression
  
  | BQuantifiedPredicate BOperatorQuantPred [BIdent] BExpression
  | BSetComprehension                       [BIdent] BExpression
  
  | BQuantifiedExpression BOperatorQuantExpr [BIdent] BExpression BExpression

  | BIdentifier BIdent BSuffix
  | BNumber Integer
  
  | BSetExtension      [BExpression]
  | BSequenceExtension [BExpression]
  
  | BParenExpression BExpression
  
  deriving(Show, Read, Eq)

data BSuffix = BCurrent | BPrevious deriving(Show, Read, Eq)
  
data BOperatorQuantPred
  = BUniversal
  | BExistential
  deriving(Show, Read, Eq)
  
data BOperatorUnaExpr
  = BOpposite
  | BInverse
  deriving(Show, Read, Eq)
  
data BOperatorBinExpr
  = BConjunction
  | BDisjunction
  | BImplication
  | BEquivalence
  
  | BEquality
  | BNonEquality
  | BMembership
  | BNonMembership
  | BInclusion
  | BStrictInclusion
  | BNonInclusion
  | BNonStrictInclusion
  | BInequality
  | BStrictInequality
  | BReverseInequality
  | BStrictReverseInequality
  
  | BAddition
  | BSubstration
  | BAsterisk
  | BDivision
  | BModulo
  | BPower
  | BInterval
  | BUnion
  | BIntersection
  | BRelation
  | BComposition
  | BDirectProduct
  | BParallelProduct
  | BDomainRestriction
  | BDomainSubstraction
  | BRangeRestriction
  | BRangeSubstraction
  | BOverloading
  | BPartialFunction
  | BTotalFunction
  | BPartialInjection
  | BTotalInjection
  | BPartialSurjection
  | BTotalSurjection
  | BTotalBijection
  | BConcatenation
  | BHeadInsertion
  | BTailInsertion
  | BHeadRestriction
  | BTailRestriction
  | BMapsToPair
  | BCommaPair
  deriving(Show, Read, Eq)

data BOperatorQuantExpr
  = BSum
  | BProduct
  | BQuantifiedUnion
  | BQuantifiedIntersection
  | BLambdaExpression
  deriving(Show, Read, Eq)
  
data BOperatorApply
  = BImage
  | BApplication
  deriving(Show, Read, Eq)
