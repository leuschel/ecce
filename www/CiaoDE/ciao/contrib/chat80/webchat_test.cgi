#!/bin/sh
exec ciao-shell $0 "$@" # -*- mode: ciao; -*-

/* CIAO WEBCHAT HTML main */

:- use_module('top/wtop').		% top level

main(_) :- hi.
