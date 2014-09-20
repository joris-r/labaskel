
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
  quickCheckWith stdArgs{maxSize=3, maxSuccess=100} testParsing
  quickCheckWith stdArgs{maxSize=10, maxSuccess=100} prop_prettyPrintAndParseAreInverse
      
toy s = case P.parse readBFile "B file" s of
        Right c -> putStrLn $ show $ prettyBComponent c
        Left e -> putStrLn $ show e