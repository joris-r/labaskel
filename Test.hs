
import Test.QuickCheck

import Lexer
import Parser
import ParserTest
import ParserProp
import Pretty
import BTree

parseFile :: FilePath -> IO()
parseFile x = do
  input <- readFile x
  putStr . show . prettyBComponent . parse . scan $ input

printBTreeFile :: FilePath -> IO()
printBTreeFile f = do
  input <- readFile f
  putStrLn . show . prettyBComponent $ (read input)
  
{- Doesn't work ...
giveSome :: IO [String]
giveSome = do
  xs <- sample' (arbitrary::Gen BComponent)
  xs :: [BComponent]
  let xs' = map (show . prettyBComponent) xs
  return xs'
-}

main = do
  quickCheckWith stdArgs{maxSize=20, maxSuccess=1000} prop_prettyPrintAndParseAreInverse