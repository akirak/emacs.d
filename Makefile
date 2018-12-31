.PHONY: update emacsql-sqlite

update:
	git pull
	git submodule update --init --recursive
	emacs -Q --batch --load init.el

# FIXME
emacsql-sqlite: straight/repos/emacsql/emacsql-sqlite.elc

# FIXME
straight/repos/emacsql/emacsql-sqlite.elc:
	cd .emacs.d/straight/repos/emacsql
	make sqlite/emacsql-sqlite
	make emacsql-sqlite.elc
