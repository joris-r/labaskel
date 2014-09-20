run: Test
	./Test

Test: Test.hs BTree.o Lexer.o Parser.o ParserTest.o Pretty.o ParserProp.o Parsing.o
	ghc --make Test

clean:
	rm -f *.hi *.o Lexer.hs Parser.hs Parser.info Test

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y

BTree.o: BTree.hs
	ghc BTree.hs

Lexer.o: Lexer.hs
	ghc Lexer.hs

Parser.o: Parser.hs
	ghc Parser.hs

ParserTest.o: ParserTest.hs
	ghc ParserTest.hs

ParserProp.o: ParserProp.hs
	ghc ParserProp.hs

Pretty.o: Pretty.hs
	ghc Pretty.hs

Parsing.o: Parsing.hs
	ghc Parsing.hs