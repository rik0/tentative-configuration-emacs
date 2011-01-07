#EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
EMACS=emacs

usage:
	echo "USAGE: make [foo.el | bar.elc]"

.el.elc:
	$(EMACS) -q --no-site-file -batch \
		-eval "(add-to-list 'load-path \".\")" \
		-f batch-byte-compile $*.el

boxquote.el:
	wget http://www.davep.org/emacs/boxquote.el
