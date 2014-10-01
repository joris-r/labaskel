
import Test.QuickCheck

import ParserTest
import Pretty
import BTree
import Parsing

parseFile :: FilePath -> IO()
parseFile path = do
  input <- readFile path
  case runBParser path input of
       Right c -> putStrLn $ show $ prettyBComponent c
       Left e -> putStrLn $ show e
  
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
  let res = runBParser "B parser test" . show . prettyBComponent $ btree in
    case res of
      Right b -> btree == b
      _ -> False
  
main = do
  quickCheckWith stdArgs{maxSize=5, maxSuccess=10000} testParsing
  
-- TODO add test with prettyPrinting without spaces
-- TODO add test with prettyPrinting with minimal parenthesis
-- TODO add parenthesis to the BTree and redo test (it's currently a weak testing)
-- TODO add testing for "MACHINE m PROPERTIES A=2+f(x) END" 
--      that should give 2+(f(x)) and not (2+f)(x)
--      not tested because of extra parenthesis from the prettyPrinter
-- TODO "MACHINE m INITIALISATION y := xx( yy(1) ) END" doesn't work
-- TODO "MACHINE m INITIALISATION x := f(x,x) END" is not tested

toy s = case runBParser "B file" s of
        Right c -> putStrLn $ show $ prettyBComponent c
        Left e -> putStrLn $ show e
        
toyTmp = do
  input <- readFile "tmp"
  let tree = read input :: BComponent
  let src = show . prettyBComponent $ tree
  putStrLn src
  toy src
  