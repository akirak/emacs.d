USER_EMACS_DIR = $(shell pwd)
export NIX_PATH = $(HOME)/.nix-defexpr/channels
export HOME_MANAGER_CONFIG= $(USER_EMACS_DIR)/nix/home.nix

build: tangle update-submodules
	home-manager switch
	$(MAKE) emacsql-sqlite || echo "emacs-sqlite failed to build."

# TODO: Use nix to build the executable
#
# On WSL, proper versions of crtl.o and crti.o are missing, so
# emacsql-sqlite fails to build.
emacsql-sqlite: update-submodules
	which gcc || nix-env -i gcc
	which as || nix-env -i binutils
	PATH="$(PWD)/extras/bin:$(PATH)" $(MAKE) -C contrib/emacsql sqlite/emacsql-sqlite emacsql-sqlite.elc

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
	if [ -d .git ]; then git config --add core.hooksPath .githooks; fi

windows-deps:
	choco install --yes mingw
	which rg || choco install --yes ripgrep
	which pandoc || choco install --yes pandoc

clear:
	rm -rf straight/repos straight/build .cache

test:
	$(MAKE) -f test.mk all
	$(MAKE) -C nix -f test/test.mk all

.PHONY:	build update-submodules tangle init clear test install-hooks windows-deps emacsql-sqlite
