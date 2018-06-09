xinitrc := $(shell readlink -f exwm/xinitrc.sh)

.PRUNE: install-exwm

install-exwm:
	ln -svf $(xinitrc) $(HOME)/.xinitrc
