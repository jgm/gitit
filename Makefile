all:
	cabal new-configure -fplugins --prefix=/usr/local
	cabal new-build

install:
	cabal new-copy

.PHONY: all clean install
