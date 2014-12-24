#!/usr/bin/env bash
if [[ -S /tmp/emacs$(id -u)/server ]]; then
    if [[ $# -eq 0 ]]; then
        #TODO: move emacs to front
        echo emacs is already running.
    else
        emacsclient -n "$@" > ~/log/emacsclient.log 2>&1 &
    fi
else
    emacs "$@" > ~/log/emacs.log 2>&1 &
fi
