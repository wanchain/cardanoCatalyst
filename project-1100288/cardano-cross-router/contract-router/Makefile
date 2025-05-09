
.PHONY: help
help:
	@echo "use: make <command>"
	@echo
	@echo "Where <command>:"
	@echo "  help           -- show this help"
	@echo "  shell          -- opens a development shell for the smart contract project"
	@echo "  code           -- start a vscode instance in the nix development shell"
	@echo "  clean          -- cabal clean"
	@echo "  build          -- build the smart contracts"
	@echo "  test           -- run the smart contract tests"
	@echo "  ci             -- run ci job"
	@echo "  export_minswap -- exports minswap v2 set of contracts to the exported directory"

.PHONY: shell
shell:
	nix develop --accept-flake-config --extra-experimental-features ca-derivations

.PHONY: code
code:
	nix-shell --command "code ."

.PHONY: clean
clean:
	cabal clean

.PHONY: build
build:
	cabal build

.PHONY: test
test:
	cabal test -f development --test-show-details=direct

.PHONY: ci
ci:
	nix flake check --accept-flake-config --extra-experimental-features ca-derivations

.PHONY: export_minswap
export_minswap:
	cabal clean && cabal run contract-router -- minswap-v2

.PHONY: export_minswap_testnet
export_minswap_testnet:
	cabal clean && cabal run contract-router --flag testnet -- minswap-v2
