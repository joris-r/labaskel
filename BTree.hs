module BTree where


-- TODO use a non empty list type
-- TODO factorise clauses and substitution
-- TODO litteral such as BOOL, TRUE, INT are not in the BTree (only as a BIdentifier)

newtype BIdent = BIdent String
  deriving(Show, Read, Eq)

unBIdent :: BIdent -> String
unBIdent (BIdent a) = a

data BComponent
  = BMachine BIdent [BClause] -- TODO use another type than [a]
  | BRefinement BIdent [BClause]
  | BImplementation BIdent [BClause]
  deriving(Show, Read, Eq)
  
data BClause
  = BAbstractVariables [BIdent]
  | BConcreteVariables [BIdent]
  | BConcreteConstants [BIdent]
  | BAbstractConstants [BIdent]
  | BPromotes [BIdent]
  | BRefines BIdent
  | BImports BIdent
  | BSees BIdent
  | BSetClause [BSetDeclaration]
  | BProperties BPredicate
  | BInvariant BPredicate
  | BAssertions [BPredicate]
  | BValues [BPredicate]
  | BInitialisation BSubstitution
  | BOperations [BOperation]
  | BLocalOperations [BOperation]
  deriving(Show, Read, Eq)

data BSetDeclaration
  = BCarrierSet BIdent
  | BEnumeratedSet [BIdent]
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
  = BUnaryPredicate BOperatorUnaPred BPredicate
  | BBinaryPredicate BOperatorBinPred BPredicate BPredicate
  | BQuantifiedPredicate BOperatorQuantPred [BIdent] BPredicate
  | BComparisonPredicate BOperatorBinPredTerm BExpression BExpression
  deriving(Show, Read, Eq)
  
data BExpression
  = BIdentifier BIdent BSuffix
  | BBoolConversion BPredicate
  | BNumber Integer
  | BUnaryExpression BOperatorUnaExpr BExpression
  | BBinaryExpression BOperatorBinExpr BExpression BExpression
  | BQuantifiedExpression BOperatorQuantExpr [BIdent] BPredicate BExpression
  | BSetComprehension [BIdent] BPredicate
  | BSetExtension [BExpression]
  | BSequenceExtension [BExpression]
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
  | BClosure
  | BNonReflexiveClosure
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
  | BPair
  | BInterval
  | BUnion
  | BIntersection
  | BRelation
  | BLeftProjection
  | BRightProjection
  | BComposition
  | BDirectProduct
  | BParallelProduct
  | BIteration
  | BImage
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
  | BApplication
  | BConcatenation
  | BHeadInsertion
  | BTailInsertion
  | BHeadRestriction
  | BTailRestriction
  deriving(Show, Read, Eq)

data BOperatorQuantExpr
  = BSum
  | BProduct
  | BQuantifiedUnion
  | BQuantifiedIntersection
  | BLambdaExpression
  deriving(Show, Read, Eq)