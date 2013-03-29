all:
	cabal install
run:
	PORT=3012 .virthualenv/cabal/bin/obvious
