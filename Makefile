install: install-all

install-all:
	ln -sf $(realpath .emacs) $(HOME)
	ln -sf $(realpath .emacs.d) $(HOME)
