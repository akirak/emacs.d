build: emacsql-sqlite post-install install-hooks

# On WSL, proper versions of crtl.o and crti.o are missing, so
# emacsql-sqlite fails to build.
emacsql-sqlite:
	PATH="$(PWD)/extras/bin:$(PATH)" nix-shell -p gnumake gcc binutils \
	--run "make -C contrib/emacsql sqlite/emacsql-sqlite emacsql-sqlite.elc"

install-hooks:
	if [ -d .git ]; then git config core.hooksPath .githooks; fi

post-install:
	emacs --batch --load post-install.el

.PHONY:	build emacsql-sqlite post-install install-hooks
