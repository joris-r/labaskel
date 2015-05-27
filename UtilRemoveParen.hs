module UtilRemoveParen
  ( removeParenComp
  , removeParenExpr
  )
where
  
import BTree
import BRec

removeParenComp = rewriteBComponent removeParenthesis
removeParenExpr = rewriteBExpression removeParenthesis

removeParenthesis = defaultMut
  { mutExpression = mutExpr
  } where
      mutExpr _ (BParenExpression p) = p
      mutExpr _ x = x


