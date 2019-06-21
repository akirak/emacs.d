USER_EMACS_DIR = $(shell pwd)

build: tangle
	$(MAKE) emacsql-sqlite || echo "emacs-sqlite failed to build."

# On WSL, proper versions of crtl.o and crti.o are missing, so
# emacsql-sqlite fails to build.
emacsql-sqlite: update-submodules
	which gcc || nix-env -i gcc
	which as || nix-env -i binutils
	PATH="$(PWD)/extras/bin:$(PATH)" $(MAKE) -C \
	contrib/emacsql sqlite/emacsql-sqlite emacsql-sqlite.elc

update-submodules:
	if [ -d .git ]; then git submodule update --init --recursive; fi

tangle:
	nix-shell '<nixpkgs>' -p emacs --run \
	'emacs --batch -l ob-tangle -eval "(org-babel-tangle-file \"README.org\")"'

init: update-submodules install-hooks
	$(MAKE) -C nix init
	$(MAKE) build
	$(MAKE) post-install

install-hooks:
	if [ -d .git ]; then git config core.hooksPath .githooks; fi

post-install:
	emacs --batch --load post-install.el

windows-deps:
	choco install --yes mingw
	which rg || choco install --yes ripgrep
	which pandoc || choco install --yes pandoc

clear:
	rm -rf straight/repos straight/build .cache

test:
	$(MAKE) -f test.mk all

.PHONY:	build update-submodules tangle init clear test \
install-hooks windows-deps emacsql-sqlite
