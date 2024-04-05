GHC = ghc
GHC_FLAGS = -o Sokoban -main-is SokobanMain -dynamic

SOURCES = SokobanMain.hs SokobanDataTypes.hs SokobanSolver.hs SokobanInput.hs

all: $(SOURCES)
	$(GHC) $(GHC_FLAGS) $(SOURCES)

clean:
	rm -f Sokoban *.o *.hi
