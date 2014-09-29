
import Test.QuickCheck

import Lexer
import Parser
import ParserTest
import ParserProp
import Pretty
import BTree
import Parsing
import qualified Text.Parsec as P

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

testParsing :: BComponent -> Bool
testParsing btree =
  let res = P.parse readBFile "B parser test" . show . prettyBComponent $ btree in
    case res of
      Right b -> btree == b
      _ -> False
  
main = do
  quickCheckWith stdArgs{maxSize=5, maxSuccess=10000} testParsing
  quickCheckWith stdArgs{maxSize=5, maxSuccess=10000} prop_prettyPrintAndParseAreInverse
  
-- TODO add test with prettyPrinting without spaces
-- TODO add test with prettyPrinting with minimal parenthesis
-- TODO add parenthesis to the BTree and redo test (it's currently a weak testing)
      
toy s = case P.parse readBFile "B file" s of
        Right c -> putStrLn $ show $ prettyBComponent c
        Left e -> putStrLn $ show e
        
toyTmp = do
  input <- readFile "tmp"
  let tree = read input :: BComponent
  let src = show . prettyBComponent $ tree
  putStrLn src
  toy src
  