sources = $(wildcard *.hs)
binaries = $(basename $(sources))

GHC_OPTS := -O2
GHC_OPTS += -W
GHC_OPTS += -XNoMonomorphismRestriction

all: $(binaries)

lint:
	/Users/peter/Library/Haskell/bin/hlint $(sources)

clean: tidy
	rm -f $(binaries)
	rm -f *.hi
	rm -f *.o

tidy:
	rm -f *~

publish: skirt
	cp skirt ~/bin/

%: %.hs
	ghc $(GHC_OPTS) $<
