module ParserProp where

import BTree
import Parser
import Lexer
import Pretty
import Test.QuickCheck
import ParserTest


prop_prettyPrintAndParseAreInverse :: BComponent -> Bool
prop_prettyPrintAndParseAreInverse btree =
  btree == (parse . scan . show . prettyBComponent $ btree)

prop_LitteralNumberAreJustDigit :: BExpression -> Bool
prop_LitteralNumberAreJustDigit (BNumber n) = 0 <= n
prop_LitteralNumberAreJustDigit _ = True

-- TODOpropriété de l'unicité des clauses identiques des composants?