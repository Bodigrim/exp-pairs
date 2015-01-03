ExpPairs: ExpPairs.hs ExpPairs/*.hs
	ghc -O2 ExpPairs.hs

tests: tests.hs ExpPairs/*.hs ExpPairs/Tests/*.hs
	ghc -O2 tests.hs

all: ExpPairs tests

test: tests
	./tests

clean:
	find -iname "*.hi" -delete -or -iname "*.o" -delete
	rm ExpPairs tests
