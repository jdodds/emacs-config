#!/usr/bin/env bash

function find_path () {
    readlink="readlink"
    if [[ $(uname) == "Darwin" ]]; then
        PATH=$PATH:/opt/local/bin
        readlink="greadlink"
    fi
    $readlink -f "$1"
}

LOGFILE=$(find_path .)/install.log

function log_command () {
    $1 | tee -a $LOGFILE
}

if [[ ! -f /usr/local/share/emacs/site-lisp/magit.el ||
	    $1 == "rebuild" ||
	    $1 == "rebuild-magit" ]]; then
    pushd magit
    log_command "make" &&  log_command "sudo make install"
    popd
fi

to_copy=(./djcb-elisp/themes/zenburn-theme.el)
for f in ${to_copy[@]}; do
    target=$(find_path $HOME/.unix-confs/emacs-config/.emacs.d)
    original=$(find_path $f)
    cp -f $original $target/
done

to_link=(.emacs .emacs.d)
# symlink everything in $to_link to $(HOME) that isn't already linked, and make
# a diff file and $link.new file if something else is sitting there.
for link in ${to_link[@]}; do
    target=$HOME/$link
    original=$(find_path $link)

    if [ ! -L $target ]; then
        if [[ -f $target && $(diff $original $target) ]]; then
            newtarget=$target.new
            difftarget=$target.diff
            echo "installing $target as $newtarget, check $(find_path $difftarget)"
            diff -u $original $target > $difftarget
            target=$target.new
        fi
        ln -sf $original $target
    fi
done
