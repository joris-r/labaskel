run: Test
	./Test

Test: Test.hs BTree.o ParserTest.o Pretty.o Parsing.o
	ghc --make Test

clean:
	rm -f *.hi *.o Test

BTree.o: BTree.hs
	ghc BTree.hs

ParserTest.o: ParserTest.hs
	ghc ParserTest.hs

Pretty.o: Pretty.hs
	ghc Pretty.hs

Parsing.o: Parsing.hs
	ghc Parsing.hs