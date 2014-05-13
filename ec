#!/usr/bin/env bash
if [[ $# -gt 0 ]]; then  #emacsclient requires file to visit
    emacsclient -n -a emacs "$@" > ~/log/emacsclient.log 2>&1 &
else
    emacs > ~/log/emacs.log 2>&1 &
fi
