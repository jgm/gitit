all:
	cabal configure -fplugins --prefix=/usr/local
	cabal build

install:
	cabal copy

.PHONY: all clean install
