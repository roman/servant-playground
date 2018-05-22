vendor/rio/README.md:
	git submodule update --init

ghcid: vendor/rio/README.md ## ghcid session for fast feedback loop
	ghcid --command "stack ghci servant-playground:lib servant-playground:exe:servant-playground --flag servant-playground:dev" --test "DevelMain.main"
.PHONY: ghcid

vendor: vendor/rio/README.md ## download vendor projects
.PHONY: vendor

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
