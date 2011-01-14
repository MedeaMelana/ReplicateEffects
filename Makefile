default: run

run:
	ghci -Wall Control.Replicate

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/ReplicateEffects/index.html
