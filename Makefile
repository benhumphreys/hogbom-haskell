EXENAME = Main

all:	$(EXENAME).hs
		ghc -W -O2 -o $(EXENAME) $(EXENAME).hs

clean:
		rm -f $(EXENAME) *.o *.hi
