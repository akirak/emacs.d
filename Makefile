build: install-hooks

install-hooks:
	if [ -d .git ]; then git config core.hooksPath .githooks; fi

.PHONY:	build install-hooks
