GHC = ghc
SOURCES = $(wildcard **/*.hs)
OBJECTS = $(wildcard *.hi *.o)
TESTS = $(wildcard test/Unit/*.hs)
DRIVER = dvcs.hs
TARGET = dvcs

DEPENDENCIES = Diff-0.4.0 aeson-1.4.6.0 random-strings-0.1.1.0 directory

CLEANUP_TEST = rm -rf .dvcs ~/test_repo

.PHONY: test

default: $(TARGET)

dvcs: $(SOURCES)
	$(GHC) $(DRIVER)

install:
	@echo "This may take some time. Please be patient ...\n"
	cabal update
	cabal install $(DEPENDENCIES)
	sudo apt-get install ssh # might not work on MAC OSX

test: $(TESTS)
	$(CLEANUP_TEST)

	@echo "\nRunning tests for SoftwareDecision.Concept.Repo Module ...\n"
	runhaskell test/Unit/RepoTest.hs;$(CLEANUP_TEST)

	@echo "\nRunning tests for SoftwareDecision.Concept.Commit Module ...\n"
	runhaskell test/Unit/CommitTest.hs;$(CLEANUP_TEST)

	@echo "\nRunning tests for SoftwareDecision.Concept.TrackedSet Module ...\n"
	runhaskell test/Unit/TrackedSetTest.hs;$(CLEANUP_TEST)

	@echo "\nRunning tests for SoftwareDecision.Communication Module ...\n"
	@echo "NOTE: You have to enter credentials 3 times for the 3 tests in Communication Module\n"
	runhaskell test/Unit/CommunicationTest.hs;$(CLEANUP_TEST)

	@echo "\nRunning tests for BehaviorHiding.Functionality Module ...\n"
	@echo "NOTE: You have to enter credentials 2 times for the dvcs clone tests\n"
	runhaskell test/Unit/FunctionalityTest.hs;$(CLEANUP_TEST)

	@echo "\nRunning tests for BehaviorHiding.UserInteraction Module ...\n"
	runhaskell test/Unit/UserInteractionTest.hs;$(CLEANUP_TEST)

clean:
	rm -rf $(TARGET) .dvcs
	find . -name \*.hi -type f -delete
	find . -name \*.o -type f -delete
