#!/usr/bin/env bash
#if [[ $# -gt 0 ]]; then  #emacsclient requires file to visit
if [[ -S /tmp/emacs*/server ]]; then
    if [[ $# -gt 0 ]]; then
        echo emacs is already running.
    else
        emacsclient -n "$@" > ~/log/emacsclient.log 2>&1 &
    fi
else
    emacs "$@" > ~/log/emacs.log 2>&1 &
fi
