all:
	cabal v2-configure -fplugins --prefix=/usr/local
	cabal v2-build

install:
	cabal v2-copy

.PHONY: all clean install
