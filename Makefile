ExpPairs: ExpPairs.hs ExpPairs/*.hs
	ghc -O2 ExpPairs.hs

Tests: Tests.hs ExpPairs/*.hs ExpPairs/Tests/*.hs
	ghc -O2 Tests.hs

all: ExpPairs Tests

test: Tests
	./Tests

clean:
	find -iname "*.hi" -delete -or -iname "*.o" -delete
	rm ExpPairs Tests
