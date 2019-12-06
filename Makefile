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

test: $(TESTS)
	runhaskell test/Unit/RepoTest.hs
	runhaskell test/Unit/CommitTest.hs

clean:
	rm -f $(TARGET)
	find . -name \*.hi -type f -delete
	find . -name \*.o -type f -delete
	rm -rf .dvcs/*
