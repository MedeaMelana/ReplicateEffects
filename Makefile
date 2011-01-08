default: run

run:
	ghci -Wall ParsecEx

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/PermuteEffects/index.html
