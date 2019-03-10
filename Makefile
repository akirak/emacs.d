USER_EMACS_DIR = $(shell pwd)
export NIX_PATH = $(HOME)/.nix-defexpr/channels
export HOME_MANAGER_CONFIG= $(USER_EMACS_DIR)/nix/home.nix

build: tangle update-submodules
	home-manager switch

update-submodules:
	if [ -d .git ]; then git submodule update --init --recursive; fi

tangle:
	nix-shell '<nixpkgs>' -p emacs --run \
	'emacs --batch -l ob-tangle -eval "(org-babel-tangle-file \"README.org\")"'

init: update-submodules install-hooks
	nix-channel --update
	nix-shell '<home-manager>' -A install
	$(MAKE) build

install-hooks:
	if [ -d .git ]; then \
		cp -fv -t .git/hooks meta/git-hooks/pre-push; \
	fi

clear:
	rm -rf straight/repos straight/build .cache

test:
	$(MAKE) -f test.mk all
	$(MAKE) -C nix -f test/test.mk all

.PHONY:	build update-submodules tangle init clear test install-hooks
