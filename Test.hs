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
--      not tested because of extra parenthesis from the prettyPrinter

  quickCheckWith stdArgs{maxSuccess=1} $
    show (runBParser "test" "MACHINE m PROPERTIES A=2+f(x) END")
    ==
    show (runBParser "test" "MACHINE m PROPERTIES A=2+(f(x)) END")

    
-- TODO "MACHINE m INITIALISATION y := xx( yy(1) ) END" is not tested

  quickCheckWith stdArgs{maxSuccess=1} $
    show (runBParser "test" "MACHINE m INITIALISATION y := xx( yy(1) ) END")
    ==
    show (runBParser "test" "MACHINE m INITIALISATION y := xx( (yy(1)) ) END")
         
         
-- TODO "MACHINE m INITIALISATION x := f(x,x) END" is not tested

  quickCheckWith stdArgs{maxSuccess=1} $
    show (runBParser "test" "MACHINE m INITIALISATION x := f(x,x) END")
    ==
    show (runBParser "test" "MACHINE m INITIALISATION x := f((x,x)) END")
               
  quickCheckWith stdArgs{maxSuccess=1} $
    show (runBParser "test" "MACHINE m INITIALISATION x := f(2+x) END")
    ==
    show (runBParser "test" "MACHINE m INITIALISATION x := f((2+x)) END")
               
  quickCheckWith stdArgs{maxSuccess=1} $
    show (runBParser "test" "MACHINE m INITIALISATION x := {a,b,c,d |- 1=2} END")
    ==
    show (runBParser "test" "MACHINE m INITIALISATION x := { a, b, c, d | ((-1) = 2) } END")
               
toy s = case runBParser "B file" s of
        Right c -> putStrLn $ show $ prettyBComponent c
        Left e -> putStrLn $ show e
        
toyTmp = do
  input <- readFile "tmp"
  let tree = read input :: BComponent
  let src = show . prettyBComponent $ tree
  putStrLn src
  toy src
  