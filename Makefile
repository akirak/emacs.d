build: post-install install-hooks

install-hooks:
	if [ -d .git ]; then git config core.hooksPath .githooks; fi

post-install:
	emacs --batch --load post-install.el

.PHONY:	build post-install install-hooks
