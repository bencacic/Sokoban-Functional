GHC = ghc
GHC_FLAGS = -o Sokoban -main-is Main -dynamic

SOURCES = Main.hs SokobanDataTypes.hs SokobanSolver.hs SokobanInput.hs SokobanTest.hs

all: $(SOURCES)
	$(GHC) $(GHC_FLAGS) $(SOURCES)

clean:
	rm -f Sokoban *.o *.hi
