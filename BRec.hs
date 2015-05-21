--    Copyright 2015 Joris Rehm
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

module BRec
  ( rewriteBComponent
  , rewriteBSubstitution
  , rewriteBPredicate
  , rewriteBExpression
  , BNode(..)
  , defaultMut
  , Mut
  , mutComponent
  , mutClause
  , mutOperation
  , mutSubstitution
  , mutPredicate
  , mutExpression
  ) where

import BTree


-- TODO find a way to test this thing. For example ensuring le current element
-- is a child of the head of the BNode list.
-- Except that's false!! Because the rewriting is bottom-up so the current
-- element contain already mutated children.


data BNode
  = BNComponent BComponent
  | BNClause BClause
  | BNOperation BOperation
  | BNSubstitution BSubstitution
  | BNPredicate BPredicate
  | BNExpression BExpression

data Mut = Mut
  { mutComponent :: BComponent -> BComponent
  , mutClause :: [BNode] -> BClause -> BClause
  , mutOperation :: [BNode] -> BOperation -> BOperation
  , mutSubstitution :: [BNode] -> BSubstitution -> BSubstitution
  , mutPredicate :: [BNode] -> BPredicate -> BPredicate
  , mutExpression :: [BNode] -> BExpression -> BExpression
  }

defaultMut = Mut
  { mutComponent = id
  , mutClause = \x -> id
  , mutOperation = \x -> id
  , mutSubstitution = \x -> id
  , mutPredicate = \x -> id
  , mutExpression = \x -> id
  }
  
rewriteBComponent :: Mut -> BComponent -> BComponent
rewriteBComponent m n@(BComponent kind name clauses)
  = (mutComponent m) (BComponent kind name clauses')
  where
    clauses' = fmap (g m [BNComponent n]) clauses

g :: Mut -> [BNode] -> BClause -> BClause
g m l n@(BProperties p)
  = (mutClause m) l (BProperties (h m ((BNClause n):l) p))
g m l n@(BInvariant p)
  = (mutClause m) l (BInvariant (h m ((BNClause n):l) p))
g m l n@(BAssertions ps)
  = (mutClause m) l (BAssertions ps')
  where ps' = fmap (h m ((BNClause n):l)) ps
g m l n@(BValues ps)
  = (mutClause m) l (BValues ps')
  where ps' = fmap (h m ((BNClause n):l)) ps
g m l n@(BInitialisation sub)
  = (mutClause m) l (BInitialisation (i m ((BNClause n):l) sub))
g m l n@(BOperations os)
  = (mutClause m) l (BOperations os')
  where os' = fmap (j m ((BNClause n):l)) os
g m l n@(BLocalOperations os)
  = (mutClause m) l (BLocalOperations os')
  where os' = fmap (j m ((BNClause n):l)) os
g m l n
  = (mutClause m) l n

j :: Mut -> [BNode] -> BOperation -> BOperation
j m l n@(BOperation outs name ins sub)
  = (mutOperation m) l (BOperation outs name ins (i m ((BNOperation n):l) sub))
  
rewriteBSubstitution m = i m []
  
i :: Mut -> [BNode] -> BSubstitution -> BSubstitution
i m l n@(BSubstitutionBlock sub)
  = (mutSubstitution m) l (BSubstitutionBlock (i m ((BNSubstitution n):l) sub))
i m l n@(BSubstitutionSimple names es)
  = (mutSubstitution m) l (BSubstitutionSimple names es')
  where es' = fmap (k m ((BNSubstitution n):l)) es
i m l n@(BSubstitutionPreCondition p sub)
  = (mutSubstitution m) l (BSubstitutionPreCondition p' sub')
  where p' = h m ((BNSubstitution n):l) p
        sub' = i m ((BNSubstitution n):l) sub
i m l n@(BSubstitutionAssert p sub)
  = (mutSubstitution m) l (BSubstitutionAssert p' sub')
  where p' = h m ((BNSubstitution n):l) p
        sub' = i m ((BNSubstitution n):l) sub
i m l n@(BSubstitutionChoice subs)
  = (mutSubstitution m) l (BSubstitutionChoice subs')
  where subs' = fmap (i m ((BNSubstitution n):l)) subs
i m l n@(BSubstitutionCond op listPredSub maybeSub)
  = (mutSubstitution m) l (BSubstitutionCond op listPredSub' maybeSub')
  where (ps,subs) = unzip listPredSub
        ps' = fmap (h m ((BNSubstitution n):l)) ps
        subs' = fmap (i m ((BNSubstitution n):l)) subs
        listPredSub' = zip ps' subs'
        maybeSub' = fmap (i m ((BNSubstitution n):l)) maybeSub
i m l n@(BSubstitutionCase e listExpSub maybeSub)
  = (mutSubstitution m) l (BSubstitutionCase e' listExpSub' maybeSub')
  where e' = k m ((BNSubstitution n):l) e
        (es, subs) = unzip listExpSub
        es' = fmap (k m ((BNSubstitution n):l)) es
        subs' = fmap (i m ((BNSubstitution n):l)) subs
        listExpSub' = zip es' subs'
        maybeSub' = fmap (i m ((BNSubstitution n):l)) maybeSub
i m l n@(BSubstitutionSpecVar op names p sub)
  = (mutSubstitution m) l (BSubstitutionSpecVar op names p' sub')
  where p' = h m ((BNSubstitution n):l) p
        sub' = i m ((BNSubstitution n):l) sub
i m l n@(BSubstitutionBecomeIn names e)
  = (mutSubstitution m) l (BSubstitutionBecomeIn names (k m ((BNSubstitution n):l) e))
i m l n@(BSubstitutionSuchThat names p)
  = (mutSubstitution m) l (BSubstitutionSuchThat names (h m ((BNSubstitution n):l) p))
i m l n@(BSubstitutionVar names sub)
  = (mutSubstitution m) l (BSubstitutionVar names (i m ((BNSubstitution n):l) sub))
i m l n@(BSubstitutionCompo op lsub rsub)
  = (mutSubstitution m) l (BSubstitutionCompo op lsub' rsub')
  where lsub' = i m ((BNSubstitution n):l) lsub
        rsub' = i m ((BNSubstitution n):l) rsub
i m l n@(BSubstitutionOpeCall params name es)
  = (mutSubstitution m) l (BSubstitutionOpeCall params name es')
  where es' = map (k m ((BNSubstitution n):l)) es
i m l n@(BSubstitutionWhile cond sub inv variant)
  = (mutSubstitution m) l (BSubstitutionWhile cond' sub' inv' variant')
  where cond' = h m ((BNSubstitution n):l) cond
        sub' = i m ((BNSubstitution n):l) sub
        inv' = h m ((BNSubstitution n):l) inv
        variant' = k m ((BNSubstitution n):l) variant
i m l n
  = (mutSubstitution m) l n

  
rewriteBPredicate m = h m []
  
h :: Mut -> [BNode] -> BPredicate -> BPredicate
h m l n@(BUnaryPredicate op p)
  = (mutPredicate m) l (BUnaryPredicate op p')
  where p' = h m ((BNPredicate n):l) p
h m l n@(BBinaryPredicate op lp rp)
  = (mutPredicate m) l (BBinaryPredicate op lp' rp')
  where lp' = h m ((BNPredicate n):l) lp
        rp' = h m ((BNPredicate n):l) rp
h m l n@(BQuantifiedPredicate op names p)
  = (mutPredicate m) l (BQuantifiedPredicate op names p')
  where p' = h m ((BNPredicate n):l) p
h m l n@(BComparisonPredicate op le re)
  = (mutPredicate m) l (BComparisonPredicate op le' re')
  where le' = k m ((BNPredicate n):l) le
        re' = k m ((BNPredicate n):l) re
h m l n@(BParenPredicate p)
  = (mutPredicate m) l (BParenPredicate p')
  where p' = h m ((BNPredicate n):l) p

  
rewriteBExpression m = k m []
  
k :: Mut -> [BNode] -> BExpression -> BExpression
k m l n@(BBoolConversion p)
  = (mutExpression m) l (BBoolConversion p')
  where p' = h m ((BNExpression n):l) p
k m l n@(BUnaryExpression op e)
  = (mutExpression m) l (BUnaryExpression op e')
  where e' = k m ((BNExpression n):l) e
k m l n@(BBinaryExpression op le re)
  = (mutExpression m) l (BBinaryExpression op le' re')
  where le' = k m ((BNExpression n):l) le
        re' = k m ((BNExpression n):l) re
k m l n@(BApply op le re)
  = (mutExpression m) l (BApply op le' re')
  where le' = k m ((BNExpression n):l) le
        re' = k m ((BNExpression n):l) re
k m l n@(BQuantifiedExpression op names p e)
  = (mutExpression m) l (BQuantifiedExpression op names p' e')
  where p' = h m ((BNExpression n):l) p
        e' = k m ((BNExpression n):l) e
k m l n@(BSetComprehension names p)
  = (mutExpression m) l (BSetComprehension names p')
  where p' = h m ((BNExpression n):l) p
k m l n@(BSetExtension es)
  = (mutExpression m) l (BSetExtension es')
  where es' = map (k m ((BNExpression n):l)) es
k m l n@(BSequenceExtension es)
  = (mutExpression m) l (BSequenceExtension es')
  where es' = map (k m ((BNExpression n):l)) es
k m l n@(BParenExpression e)
  = (mutExpression m) l (BParenExpression e')
  where e' = k m ((BNExpression n):l) e
k m l n
  = (mutExpression m) l n




