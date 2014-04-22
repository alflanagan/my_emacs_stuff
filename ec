#!/usr/bin/env bash
if [[ $# -gt 0 ]]; then  #emacsclient requires file to visit
    emacsclient -n -a emacs -q "$@" > ~/log/emacsclient.log 2>&1 &
else
    emacs -f server-start > ~/log/emacs.log 2>&1 &
fi

#unfortunately, emacs --daemon is not working as expected
#following line is good argument for doing this in python
#EMACS_PS=$(ps -fu $USER | grep emac[s] | sed -e's/ \+/\t/g' | cut -f2)
#if [[ -z ${EMACS_PS} ]]; then
#    emacs -f server-start "$@" > ~/log/emacs.log 2>&1 &
#else
#    emacsclient -n -a emacs -q "$@" > ~/log/emacsclient.log 2>&1 &
#fi
