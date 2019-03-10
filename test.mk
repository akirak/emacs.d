all: emacs

emacs:
	which emacs
	test -d $(HOME)/.emacs.d
	test -f $(HOME)/.emacs.d/init.el

.PRUNE: all emacs
