.DEFAULT: ghcid

ghcid: ## ghcid session for fast feedback loop
	ghcid --command "stack ghci servant-playground:lib servant-playground:exe:servant-playground --flag servant-playground:dev" --test "DevelMain.main"
.PHONY: ghcid

vendor: ## download vendor projects
	git submodule update --init
.PHONY: vendor
