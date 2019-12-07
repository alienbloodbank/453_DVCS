GHC = ghc
SOURCES = $(wildcard **/*.hs)
OBJECTS = $(wildcard *.hi *.o)
TESTS = $(wildcard test/Unit/*.hs)
DRIVER = dvcs.hs
TARGET = dvcs

.PHONY: test

default: $(TARGET)

dvcs: $(SOURCES)
	$(GHC) $(DRIVER)

install:
	cabal install Diff-0.4.0 aeson-1.4.6.0 random-strings-0.1.1.0 

test: $(TESTS)
	runhaskell test/Unit/RepoTest.hs
	runhaskell test/Unit/CommitTest.hs

clean:
	rm -f $(TARGET)
	find . -name \*.hi -type f -delete
	find . -name \*.o -type f -delete
	rm -rf .dvcs
