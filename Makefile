default: submodule test

submodule:
	git submodule update --init

test:
	emacs --batch -l init.el -f batch-byte-compile init.el
