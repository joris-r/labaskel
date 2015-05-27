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

module ParserTest where

import Test.QuickCheck
import Control.Monad

import BTree
import Pretty
import Util

-- TODO do a better job for controlling the size. We need something
-- that control the depth and the width of the trees. This should be
-- done for all kind of trees such as Substitution, Predicate or Expression.
-- We need also a better control of the number of looping back from Expression
-- to predicate
-- ==> DONE, isn't is ?


instance Arbitrary BComponent where
  arbitrary = fmap addMinParenComp (liftM3 BComponent arbitrary arbitrary arbitrary)

instance Arbitrary BComponentType where
  arbitrary = elements
    [ BMachine
    , BRefinement
    , BImplementation
    ]
    
instance Arbitrary BIdent where
  arbitrary = elements . (map BIdent) $ ["x","y","z","var","toto"]

instance Arbitrary BClause where
  arbitrary = oneof
    [ liftM BAbstractVariables (listOf1 arbitrary) -- TODO relax empty ?
    , liftM BConcreteVariables (listOf1 arbitrary) -- TODO relax empty ?
    , liftM BConcreteConstants (listOf1 arbitrary) -- TODO relax empty ?
    , liftM BAbstractConstants (listOf1 arbitrary) -- TODO relax empty ?
    , liftM BPromotes (listOf1 arbitrary) -- TODO relax empty ?
    , liftM BInvariant arbitrary
    , liftM BAssertions (listOf1 (arbitrary))
    , liftM BProperties arbitrary
    , liftM BValues (listOf1 (arbitrary))
    , liftM BInitialisation arbitrary
    , liftM BRefines arbitrary
    , liftM BImports (listOf1 arbitrary)
    , liftM BSees (listOf1 arbitrary)
    , liftM BSetClause (listOf1 arbitrary)
    , liftM BOperations (listOf1 arbitrary)  -- TODO relax empty ?
    , liftM BLocalOperations (listOf1 arbitrary)  -- TODO relax empty ?
    ]
        
instance Arbitrary BSetDeclaration where
  arbitrary = oneof
    [ liftM BCarrierSet arbitrary
    , liftM2 BEnumeratedSet arbitrary (listOf1 arbitrary)
    ]
    
instance Arbitrary BOperation where
  arbitrary = oneof
    [ liftM4 BOperation arbitrary arbitrary arbitrary protectedSubstitution
    ]
    where
      -- enclose naked ";" with "BEGIN ... END" block, examples:
      -- "a ; b" becomes "BEGIN a;b END"
      -- "a || b ; c" becoms "a || BEGIN b;c END" 
      protectedSubstitution = do
        sub <- arbitrary
        return $ protect sub
      protect sub@(BSubstitutionCompo BOpSubSeq lhs rhs) = BSubstitutionBlock sub
      protect (BSubstitutionCompo BOpSubParal lhs rhs) =
        BSubstitutionCompo BOpSubParal (protect lhs) (protect rhs)
      protect x = x
    
instance Arbitrary BSubstitution where
  arbitrary = sized sizedBSub

sizedBSub :: Int -> Gen BSubstitution
sizedBSub 0 = oneof
  [ return BSubstitutionSkip
  , liftM2 BSubstitutionSimple (listOf1 arbitrary) (listOf1 arbitrary)
  , liftM2 BSubstitutionBecomeIn (listOf1 arbitrary) arbitrary
  , liftM2 BSubstitutionSuchThat (listOf1 arbitrary) arbitrary
  ]
sizedBSub n = frequency
  [ (2, liftM BSubstitutionBlock (sizedBSub $ n - 1))
  , (1, return BSubstitutionSkip)
  , (1, liftM2 BSubstitutionSimple (listOf1 arbitrary) (listOf1 arbitrary))
  , (2, liftM2 BSubstitutionPreCondition arbitrary (sizedBSub $ n - 1))
  , (2, liftM2 BSubstitutionAssert arbitrary (sizedBSub $ n - 1))
  , (2, liftM BSubstitutionChoice (vectorOf 1 (sizedBSub $ n - 1)) )
  , (2, liftM BSubstitutionChoice (vectorOf 2 (sizedBSub $ n `div` 2)) )
  , (2, liftM BSubstitutionChoice (vectorOf 3 (sizedBSub $ n `div` 3)) )
  , (2, substCond BIf n)
  , (2, substCond BSelect n)
  , (2, substCase n)
  , (2, liftM4 BSubstitutionSpecVar
          arbitrary
          (listOf1 arbitrary)
          arbitrary
          (sizedBSub $ n - 1 ) )
  , (1, liftM2 BSubstitutionBecomeIn (listOf1 arbitrary) arbitrary)
  , (1, liftM2 BSubstitutionSuchThat (listOf1 arbitrary) arbitrary)
  , (2, liftM2 BSubstitutionVar (listOf1 arbitrary) (sizedBSub $ n - 1))
  , (2, substComp BOpSubParal)
  , (2, substComp BOpSubSeq)
  , (2, liftM3 BSubstitutionOpeCall arbitrary arbitrary arbitrary)
  , (2, liftM4 BSubstitutionWhile
          arbitrary
          (sizedBSub $ n - 1)
          arbitrary
          arbitrary)
  ] where
    {-
      Parallel substitution operator has left precedence in the grammar
      and there are no way to group it in another way because there are
      no parenthesis possible (the block substitution can group them but
      they are keept in the BTree).
      Therefore we have normaly only a tree like this: (x || y) || z.
      And so we must rotate the tree in the good shape otherwise
      the btree before and after the parsing test will be different.
      Same for the sequencing operator ";".
      And both have the same precedence (that is strange in fact).
    -}
    substComp op = do
      bsubcomp <- liftM3 BSubstitutionCompo (return op)
                  (sizedBSub $ n `div` 2) (sizedBSub $ n `div` 2)
      return $ rotateLeftSubComp bsubcomp
      
    substCond :: BOperatorCond -> Int -> Gen BSubstitution
    substCond op n =
      oneof
        [ sizedBSubCond op (n-1) 1 False
        , sizedBSubCond op (n-1) 2 True
        , sizedBSubCond op (n-1) 2 False
        , sizedBSubCond op (n-1) 3 True
        , sizedBSubCond op (n-1) 3 False
        ]
    
    {- Generator for a conditional substitution ("IF ..." or "SELECT ..."
       The 1° parameter is the kind of the Conditional
       the 2° parameter is the global size of the current generation of
       the BSubstitution.
       The 3° parameter is how many branches will be generated in this IF.
       And the 4° is the presence of the ELSE -}
    sizedBSubCond :: BOperatorCond -> Int -> Int -> Bool -> Gen BSubstitution
    sizedBSubCond op m n False =
      liftM3 BSubstitutionCond (return op)
        (liftM2 zip (vectorOf n arbitrary) (vectorOf n
          (sizedBSub (m`div`n) ) ))
        (return Nothing)
    sizedBSubCond op m n True =
      liftM3 BSubstitutionCond (return op)
        (liftM2 zip (vectorOf (n-1) arbitrary) (vectorOf (n-1)
          (sizedBSub (m`div`n) ) ))
        (liftM Just (sizedBSub (m`div`n)))

    {- Same idea than substCond but for the "Case Of" (it differ by type) -}
    substCase :: Int -> Gen BSubstitution
    substCase n =
      oneof
        [ sizedBSubCase (n-1) 1 False
        , sizedBSubCase (n-1) 2 True
        , sizedBSubCase (n-1) 2 False
        , sizedBSubCase (n-1) 3 True
        , sizedBSubCase (n-1) 3 False
        ]
    sizedBSubCase :: Int -> Int -> Bool -> Gen BSubstitution
    sizedBSubCase m n False =
      liftM3 BSubstitutionCase
        (sizedBExpr (m`div`n) )
        (liftM2 zip (vectorOf n arbitrary) (vectorOf n
          (sizedBSub (m`div`n) ) ))
        (return Nothing)
    sizedBSubCase m n True =
      liftM3 BSubstitutionCase
        (sizedBExpr (m`div`n) )
        (liftM2 zip (vectorOf (n-1) arbitrary) (vectorOf (n-1)
          (sizedBSub (m`div`n) ) ))
        (liftM Just (sizedBSub (m`div`n)))

{- Rotate the tree of substitutions in order have the left precedence
   on the "||" and ";" substitution -}
rotateLeftSubComp :: BSubstitution -> BSubstitution

rotateLeftSubComp (BSubstitutionCompo op1 a (BSubstitutionCompo op2 b c)) =
  rotateLeftSubComp (BSubstitutionCompo op1 (BSubstitutionCompo op2 a b) c)
  
rotateLeftSubComp (BSubstitutionCompo op a b) =
  BSubstitutionCompo op (rotateLeftSubComp a) (rotateLeftSubComp b)
  
rotateLeftSubComp x = x


instance Arbitrary BExpression where
  arbitrary = sized sizedBExpr

sizedBExpr :: Int -> Gen BExpression
sizedBExpr 0 = oneof
  [ liftM2 BIdentifier arbitrary arbitrary
  , liftM BNumber (elements [0,1,3,25,100])
  , liftM3 BComparisonPredicate
           arbitrary
           (sizedBExpr $ 0)
           (sizedBExpr $ 0)
  ]
sizedBExpr n = frequency
  [ (1, liftM3 BComparisonPredicate
               arbitrary
               (sizedBExpr $ (n-1) `div` 2)
               (sizedBExpr $ (n-1) `div` 2))
  , (2, liftM BNegation (sizedBExpr $ n-1))
  , (2, liftM3 BBinaryPredicate
               arbitrary
               (sizedBExpr $ (n-1) `div` 2)
               (sizedBExpr $ (n-1) `div` 2))
  , (2, liftM3 BQuantifiedPredicate
               arbitrary
               (listOf1 arbitrary)
               (sizedBExpr $ n-1))
  , (1, liftM2 BIdentifier arbitrary arbitrary)
  , (1, liftM BNumber (elements [0,1,3,25,100]))
  , (1, liftM BBoolConversion (sizedBExpr $ n-1))  -- looping back!
  , (2, liftM2 BUnaryExpression arbitrary
               (sizedBExpr $ n-1))
  , (2, liftM3 BBinaryExpression arbitrary
               (sizedBExpr $ (n-1) `div` 2) (sizedBExpr $ (n-1) `div` 2))
  , (2, liftM3 BApply arbitrary
               (sizedBExpr $ (n-1) `div` 2) (sizedBExpr $ (n-1) `div` 2))
  , (1, liftM4 BQuantifiedExpression 
               arbitrary
               (listOf1 arbitrary)
               (sizedBExpr $ n-1)  -- looping back!
               (sizedBExpr $ n-1))
  , (1, liftM2 BSetComprehension
               (listOf1 arbitrary)
               (sizedBExpr $ n-1))  -- looping back!
  , (2, liftM BSetExtension (vectorOf 1 (sizedBExpr $ (n-1))))
  , (2, liftM BSetExtension (vectorOf 2 (sizedBExpr $ (n-1) `div` 2)))
  , (2, liftM BSetExtension (vectorOf 3 (sizedBExpr $ (n-1) `div` 3)))
  , (2, liftM BSequenceExtension (vectorOf 1 (sizedBExpr $ (n-1))))
  , (2, liftM BSequenceExtension (vectorOf 2 (sizedBExpr $ (n-1) `div` 2)))
  , (2, liftM BSequenceExtension (vectorOf 3 (sizedBExpr $ (n-1) `div` 3)))
  ]

  
-- TODO is there a way to have this automatically for all enumerated types?
  
instance Arbitrary BSuffix where
  arbitrary = elements [BCurrent, BPrevious]
  
instance Arbitrary BOperatorSpecVar where
  arbitrary = elements
    [ BAny
    , BLet
    ]
    
instance Arbitrary BOperatorQuantPred where
  arbitrary = elements
    [ BUniversal
    , BExistential
    ]
    
instance Arbitrary BOperatorUnaExpr where
  arbitrary = elements
    [ BOpposite
    , BInverse
    ]

instance Arbitrary BOperatorBinExpr where
  arbitrary = elements
    [ BConjunction
    , BDisjunction
    , BImplication
    , BEquivalence
    
    , BEquality
    , BInequality
    , BMembership
    , BNonMembership
    , BInclusion
    , BStrictInclusion
    , BNonInclusion
    , BNonStrictInclusion
    , BInequality
    , BStrictInequality
    , BReverseInequality
    , BStrictReverseInequality
    
    , BAddition
    , BSubstration
    , BAsterisk
    , BDivision
    , BModulo
    , BPower
    , BInterval
    , BUnion
    , BIntersection
    , BRelation
    , BComposition
    , BDirectProduct
    , BParallelProduct
    , BDomainRestriction
    , BDomainSubstraction
    , BRangeRestriction
    , BRangeSubstraction
    , BOverloading
    , BPartialFunction
    , BTotalFunction
    , BPartialInjection
    , BTotalInjection
    , BPartialSurjection
    , BTotalSurjection
    , BTotalBijection
    , BConcatenation
    , BHeadInsertion
    , BTailInsertion
    , BHeadRestriction
    , BTailRestriction
    , BCommaPair
    , BMapsToPair
    ]
    
instance Arbitrary BOperatorQuantExpr where
  arbitrary = elements
    [ BSum
    , BProduct
    , BQuantifiedUnion
    , BQuantifiedIntersection
    , BLambdaExpression
    ]
    

instance Arbitrary BOperatorApply where
  arbitrary = elements
    [ BImage
    , BApplication
    ]
  