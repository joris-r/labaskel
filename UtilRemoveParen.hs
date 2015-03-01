module UtilRemoveParen
  ( removeParenComp
  , removeParenPred
  , removeParenExpr
  )
where
  
import BTree
import BRec

removeParenComp = rewriteBComponent removeParenthesis
removeParenPred = rewriteBPredicate removeParenthesis
removeParenExpr = rewriteBExpression removeParenthesis

removeParenthesis = defaultMut
  { mutPredicate = mutPred
  , mutExpression = mutExpr
  } where
      mutPred _ (BParenPredicate p) = p
      mutPred _ x = x
      mutExpr _ (BParenExpression p) = p
      mutExpr _ x = x


