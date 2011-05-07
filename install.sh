#!/usr/bin/env bash
LOGFILE=$(readlink -f .)/install.log

function log_command () {
    $1 | tee -a $LOGFILE
}

if [[ ! -f /usr/local/share/emacs/site-lisp/magit.el || $1 == "rebuild" ]]; then
    cd magit
    log_command "make" &&  log_command "sudo make install"
fi

to_link=(.emacs .emacs.d foo)

# symlink everything in $to_link to $(HOME) that isn't already linked, and make
# a diff file and $link.new file if something else is sitting there.
for link in ${to_link[@]}; do
    target=$HOME/$link
    original=$(readlink -f $link)

    if [ ! -L $target ]; then
	if [[ -f $target && $(diff $original $target) ]]; then
	    newtarget=$target.new
	    difftarget=$target.diff
	    echo "installing $target as $newtarget, check $(readlink -f $difftarget)"
	    diff -u $original $target > $difftarget
	    target=$target.new
	fi
	ln -sf $original $target 
    fi
done
