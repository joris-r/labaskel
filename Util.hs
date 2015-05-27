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

module Util(addMinParenComp) where

import BTree
import BRec
import UtilRemoveParen

import Data.Maybe(fromJust)
import Data.List(find, findIndex)


addMinParenComp :: BComponent -> BComponent
addMinParenComp = rewriteBComponent addMinParen

addMinParen = defaultMut
  { mutSubstitution = addMinParenSub
  , mutExpression = addMinParenExpr
  }


addMinParenSub :: [BNode] -> BSubstitution -> BSubstitution

-- add parenthesis around comma
-- in order to protect it inside expression list
-- but only for the first occurence of the pair
-- as one parenthesis protect all the inside pairs
addMinParenSub _ (BSubstitutionSimple names es)
  = BSubstitutionSimple names (map parenFirstPair es)
addMinParenSub _ (BSubstitutionOpeCall names name es)
  = BSubstitutionOpeCall names name (map parenFirstPair es)
addMinParenSub _ n = n

  
parenFirstPair n@(BBinaryExpression BCommaPair _ _) = BParenExpression n
parenFirstPair (BUnaryExpression op e) = BUnaryExpression op (parenFirstPair e)
parenFirstPair (BBinaryExpression op e e') = BBinaryExpression op (parenFirstPair e) (parenFirstPair e')
parenFirstPair n = n


data Assoc = AssocLeft | AssocRight deriving(Show, Eq)

-- TODO do a unit test with the following data


-- ( 0=0 => 1=1 ) => 2=2
-- that is also
-- 0=0 => 1=1 => 2=2
pred01 =
  BBinaryPredicate
    BImplication (
    BBinaryPredicate
      BImplication(
      predTermNumX 0)(
      predTermNumX 1))(
    predTermNumX 2)

-- 0=0 => ( 1=1 => 2=2 )
pred01' =
  BBinaryPredicate
    BImplication(
    predTermNumX 0)(
    BBinaryPredicate
      BImplication(
      predTermNumX 1)(
      predTermNumX 2))

-- ( 0=0 => 1=1 ) & 2=2
pred02 =
  BBinaryPredicate
    BConjunction (
    BBinaryPredicate
      BImplication(
      predTermNumX 0)(
      predTermNumX 1))(
    predTermNumX 2)

-- 0=0 => (1=1 & 2=2)
-- that is
-- 0=0 => 1=1 & 2=2
pred02' =
  BBinaryPredicate
    BImplication (
    predTermNumX 0)(
    BBinaryPredicate
      BConjunction(
      predTermNumX 1)(
      predTermNumX 2))

-- ( 0=0 & 1=1 ) => 2=2
-- that is also
-- 0=0 & 1=1 => 2=2
pred03 =
  BBinaryPredicate
    BImplication (
    BBinaryPredicate
      BConjunction(
      predTermNumX 0)(
      predTermNumX 1))(
    predTermNumX 2)

-- 0=0 & ( 1=1 => 2=2 )
pred03' =
  BBinaryPredicate
    BConjunction(
    predTermNumX 0)(
    BBinaryPredicate
      BImplication(
      predTermNumX 1)(
      predTermNumX 2))

-- not(0=0) => 1=1
-- that is also
-- not 0=0 => 1=1
pred04 =
  BBinaryPredicate
    BImplication(
      BNegation(
        predTermNumX 0))(
      predTermNumX 1)
    
-- not(0=0 => 1=1)
pred04' =
  BNegation(
    BBinaryPredicate
      BImplication(
      predTermNumX 0)(
      predTermNumX 1))
      
predTermNumX x =
  BComparisonPredicate
    BEquality
    (BNumber x)
    (BNumber x)


-- TODO I'm not happy with following similar code between predicate and expression

opPrioExpr =
  -- from top priority to low priority
  [ [ (BPower,AssocRight) ]
  , [ (BAsterisk,AssocLeft)
    , (BDivision,AssocLeft)
    , (BModulo,AssocLeft)
    ]
  , [ (BAddition,AssocLeft)
    , (BSubstration,AssocLeft)
    ]
  , [ (BInterval,AssocLeft)
    ]
  , [ (BMapsToPair,AssocLeft)
    , (BUnion,AssocLeft)
    , (BIntersection,AssocLeft)
    , (BDirectProduct,AssocLeft)
    , (BDomainRestriction,AssocLeft)
    , (BDomainSubstraction,AssocLeft)
    , (BRangeRestriction,AssocLeft)
    , (BRangeSubstraction,AssocLeft)
    , (BOverloading,AssocLeft)
    , (BConcatenation,AssocLeft)
    , (BHeadInsertion,AssocLeft)
    , (BTailInsertion,AssocLeft)
    , (BHeadRestriction,AssocLeft)
    , (BTailRestriction,AssocLeft)
    ]
  , [ (BRelation,AssocLeft)
    , (BPartialFunction,AssocLeft)
    , (BTotalFunction,AssocLeft)
    , (BPartialInjection,AssocLeft)
    , (BTotalInjection,AssocLeft)
    , (BPartialSurjection,AssocLeft)
    , (BTotalSurjection,AssocLeft)
    , (BTotalBijection,AssocLeft)
    ]
  , [ (BCommaPair,AssocLeft)
    ]
  , [ (BEquivalence,AssocLeft) ]
  , [ (BConjunction,AssocLeft), (BDisjunction,AssocLeft) ]
  , [ (BImplication,AssocLeft) ]
  , [ (BComposition,AssocLeft)
    , (BParallelProduct,AssocLeft)
    ]
  ]
  
rankPrioExpr :: BOperatorBinExpr -> Int
rankPrioExpr op =
  fromJust (findIndex (isInSubList op) opPrioExpr)
  where
    isInSubList op ((opX,_):l) | op == opX = True
    isInSubList op (_:l) | True  = isInSubList op l
    isInSubList op [] = False

assocExpr :: BOperatorBinExpr -> Assoc
assocExpr op = snd $ fromJust $ find f (concat opPrioExpr)
  where
    f = (==) op . fst

strictLessPrioExpr :: BOperatorBinExpr -> BOperatorBinExpr -> Bool
strictLessPrioExpr l r = (rankPrioExpr l) > (rankPrioExpr r)

samePrioExpr :: BOperatorBinExpr -> BOperatorBinExpr -> Bool
samePrioExpr l r = (rankPrioExpr l) == (rankPrioExpr r)





addMinParenExpr :: [BNode] -> BExpression -> BExpression

addMinParenExpr ((BNExpression (BBinaryExpression opAbove lAbove rAbove)):_)
                             n@(BBinaryExpression op _ _)
  | op `strictLessPrioExpr` opAbove
    = BParenExpression n
  | (op `samePrioExpr` opAbove) && (assocExpr opAbove /= (sideFromAbove))
    = BParenExpression n
  | otherwise
    = n
  where
    sideFromAbove
      | lAbove `equalModParenExpr` n = AssocLeft
      | rAbove `equalModParenExpr` n = AssocRight
      | otherwise = error "Impossible case on the side of a binary expression"

-- always add parenthesis on the argument of "not"
-- because it's specified like this on B method
-- (this parser accept without)
addMinParenExpr [] n@(BBinaryPredicate _ _ _) =
  BParenExpression n
addMinParenExpr ((BNExpression (BNegation _)):_)
                            n@(BBinaryPredicate _ _ _) = 
  BParenExpression n
addMinParenExpr ((BNExpression (BNegation _)):_)
                            n@(BNegation _) = 
  BParenExpression n
addMinParenExpr ((BNExpression (BNegation _)):_)
                            n@(BQuantifiedPredicate _ _ _) = 
  BParenExpression n
addMinParenExpr ((BNExpression (BNegation _)):_)
                            n@(BComparisonPredicate _ _ _) = 
  BParenExpression n
-- TODO is it possible to factorize those 4 declaration by
--      using a "n does not match a (BParenExpression _)" ?




addMinParenExpr _ n@(BBinaryExpression BComposition _ _)
  = BParenExpression n
addMinParenExpr _ n@(BBinaryExpression BParallelProduct _ _)
  = BParenExpression n

addMinParenExpr _ (BSetExtension es)
  = BSetExtension (map parenFirstPair es)
addMinParenExpr _ (BSequenceExtension es)
  = BSequenceExtension (map parenFirstPair es)

-- all unary expr operator have higher precedence than binary expr operator
addMinParenExpr ((BNExpression (BUnaryExpression _ _)):_) n@(BBinaryExpression _ _ _)
  = BParenExpression n

addMinParenExpr ((BNExpression (BApply _ lAbove _)):_) n@(BBinaryExpression _ _ _)
  | lAbove `equalModParenExpr` n
    = BParenExpression n
  | otherwise
    = n
    

-- TODO because of a shortcoming of the parser
addMinParenExpr _ n@(BUnaryExpression _ _) = BParenExpression n
{-
TODO when the pattern just above is removed, put the following instead

-- unary operator associativity is not implemented in the parser
addMinParenExpr ((BNExpression (BUnaryExpression _ _)):_) n@(BUnaryExpression _ _)
  = BParenExpression n

addMinParenExpr ((BNExpression (BApply _ lAbove _)):_) n@(BUnaryExpression _ _)
  | lAbove `equalModParenExpr` n
    = BParenExpression n
  | otherwise
    = n
-}

addMinParenExpr ((BNExpression (BApply _ lAbove _)):_) n@(BApply _ _ _)
  | lAbove `equalModParenExpr` n
    = BParenExpression n
  | otherwise
    = n

  
addMinParenExpr _ n
  = n


equalModParenExpr l r =
  (removeParenExpr l) == (removeParenExpr r)

