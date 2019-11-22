GHC = ghc
SOURCES = $(wildcard *.hs)
OBJECTS = $(wildcard *.hi *.o)
DRIVER = dvcs.hs
TARGET = dvcs

default: $(TARGET)

dvcs: $(SOURCES)
	$(GHC) $(DRIVER)

clean:
	rm -f $(OBJECTS) $(TARGET)
