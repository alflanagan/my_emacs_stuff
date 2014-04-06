#!/usr/bin/env bash
#following line is good argument for doing this in python
EMACS_PS=$(ps -fu $USER | grep emac[s] | sed -e's/ \+/\t/g' | cut -f2)
if [[ -z ${EMACS_PS} ]]; then
    #emacsclient -a "" -c -n "$@" would work here, but if emacs is already
    #running, creates new frame instead of reusing existing
    #without -c, opens a text frame (?) and immediately exists
    emacs "$@" > ~/log/emacs.log 2>&1 &
else
    emacsclient -n -q "$@" > ~/log/emacsclient.log 2>&1 &
fi
