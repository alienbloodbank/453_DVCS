GHC = ghc
SOURCES = $(wildcard **/*.hs)
OBJECTS = $(wildcard *.hi *.o)
TESTS = $(wildcard test/Unit/*.hs)
DRIVER = dvcs.hs
TARGET = dvcs

CLEANUP_TEST = rm -rf .dvcs ~/test_repo

.PHONY: test

default: $(TARGET)

dvcs: $(SOURCES)
	$(GHC) $(DRIVER)

install:
	cabal install Diff-0.4.0 aeson-1.4.6.0 random-strings-0.1.1.0
	sudo apt-get install ssh

test: $(TESTS)
	$(CLEANUP_TEST)
	runhaskell test/Unit/RepoTest.hs;$(CLEANUP_TEST)
	runhaskell test/Unit/CommitTest.hs;$(CLEANUP_TEST)
	runhaskell test/Unit/TrackedSetTest.hs;$(CLEANUP_TEST)
	runhaskell test/Unit/CommunicationTest.hs;$(CLEANUP_TEST)

clean:
	rm -rf $(TARGET) .dvcs
	find . -name \*.hi -type f -delete
	find . -name \*.o -type f -delete
