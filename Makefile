site.nix: site.cabal
	cabal2nix . > $@
