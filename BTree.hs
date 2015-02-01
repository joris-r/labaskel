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
  | BProperties BPredicate
  | BInvariant BPredicate
  | BAssertions [BPredicate]
  | BValues [BPredicate] -- TODO replace predicate by a list of (var,expr)
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
  | BSubstitutionPreCondition BPredicate BSubstitution
  | BSubstitutionAssert BPredicate BSubstitution
  | BSubstitutionChoice [BSubstitution]
  | BSubstitutionCond BOperatorCond [(BPredicate,BSubstitution)] (Maybe BSubstitution) -- at least one couple in the list
  | BSubstitutionCase BExpression [(BExpression,BSubstitution)] (Maybe BSubstitution) -- at least one couple in the list
  | BSubstitutionSpecVar BOperatorSpecVar [BIdent] BPredicate BSubstitution
  | BSubstitutionBecomeIn [BIdent] BExpression
  | BSubstitutionSuchThat [BIdent] BPredicate
  | BSubstitutionVar [BIdent] BSubstitution
  | BSubstitutionCompo BOperatorSubComp BSubstitution BSubstitution
  | BSubstitutionOpeCall [BIdent] BIdent [BExpression]
  | BSubstitutionWhile BPredicate BSubstitution BPredicate BExpression
  deriving(Show, Read, Eq)
  
data BOperatorCond = BIf | BSelect deriving(Show, Read, Eq)
data BOperatorSpecVar = BAny | BLet deriving(Show, Read, Eq)
data BOperatorSubComp = BOpSubParal | BOpSubSeq deriving(Show, Read, Eq)

data BPredicate
  = BUnaryPredicate BOperatorUnaPred BPredicate -- TODO replace by just a negation
  | BBinaryPredicate BOperatorBinPred BPredicate BPredicate
  | BQuantifiedPredicate BOperatorQuantPred [BIdent] BPredicate
  | BComparisonPredicate BOperatorBinPredTerm BExpression BExpression
  | BParenPredicate BPredicate
  deriving(Show, Read, Eq)
  
data BExpression
  = BIdentifier BIdent BSuffix -- TODO restrict suffix "$0" to become such as subst
  | BBoolConversion BPredicate
  | BNumber Integer
  | BUnaryExpression BOperatorUnaExpr BExpression
  | BBinaryExpression BOperatorBinExpr BExpression BExpression
  | BBuiltinCall BOperatorBuiltinCall BExpression BExpression
  | BApply BOperatorApply BExpression BExpression
  | BQuantifiedExpression BOperatorQuantExpr [BIdent] BPredicate BExpression
  | BSetComprehension [BIdent] BPredicate
  | BSetExtension [BExpression] --TODO factorize BSetExtension and BSequenceExtension ?
  | BSequenceExtension [BExpression]
  | BParenExpression BExpression
  deriving(Show, Read, Eq)

data BSuffix = BCurrent | BPrevious deriving(Show, Read, Eq)
  
data BOperatorUnaPred = BNegation deriving(Show, Read, Eq)
  
data BOperatorBinPred
  = BConjunction
  | BDisjunction
  | BImplication
  | BEquivalence
  deriving(Show, Read, Eq)  
  
data BOperatorQuantPred
  = BUniversal
  | BExistential
  deriving(Show, Read, Eq)
  
data BOperatorBinPredTerm
  = BEquality
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
  deriving(Show, Read, Eq)
  
data BOperatorUnaExpr
  = BOpposite
  | BMaximum
  | BMinimum
  | BCardinality
  | BPowerSet
  | BNonEmptyPowerSet
  | BFinitePowerSet
  | BNonEmptyFinitePowerSet
  | BGeneralizedUnion
  | BGeneralizedIntersection
  | BIdentity
  | BInverse
  | BClosure  -- TODO check number of parameters
  | BNonReflexiveClosure  -- TODO check number of parameters
  | BDomain
  | BRange
  | BFunctionTransformation
  | BRelationTransformation
  | BSequence
  | BNonEmptySequence
  | BInjectiveSequence
  | BNonEmptyInjectiveSequence
  | BPermutation
  | BSize
  | BFirst
  | BLast
  | BFront
  | BTail
  | BRev
  | BGeneralizedConcatenation
  deriving(Show, Read, Eq)
  
data BOperatorBinExpr
  = BAddition
  | BSubstration -- TODO should also be BSetSubstration
  | BAsterisk -- TODO should be BMultiplication and BCartesianProduct
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
  
data BOperatorBuiltinCall
  = BLeftProjection
  | BRightProjection
  | BIteration
  deriving(Show, Read, Eq)

data BOperatorApply
  = BImage
  | BApplication
  deriving(Show, Read, Eq)
