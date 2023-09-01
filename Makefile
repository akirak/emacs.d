build: install-hooks

install-hooks:
	if [ -d .git ]; then git config --local core.hooksPath .githooks; fi

.PHONY:	build install-hooks
