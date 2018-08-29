xinitrc := $(shell readlink -f exwm/xinitrc.sh)
ANSIBLE := ansible-playbook -c local -i localhost, playbook.yml

.PRUNE: normal deps exwm

normal:
	$(ANSIBLE)

deps:
	$(ANSIBLE) -t deps

exwm: no-exwm
	ln -svf $(xinitrc) $(HOME)/.xinitrc
