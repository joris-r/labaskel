run: Test
	./Test

Test:
	ghc --make Test

clean:
	rm -f *.hi *.o Test

