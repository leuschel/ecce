
%% Define all operators here also in chip_ops_boot.pl

%% Operators for finite domain constraints

:- op( 700,xfx,'#<').
:- op( 700,xfx,'#<=').
:- op( 700,xfx,'#>').
:- op( 700,xfx,'#>=').
:- op( 700,xfx,'#=').
:- op( 700,xfx,'::').
:- op( 700,xfx,#\=).
:- op( 500,xfx,'..').
:- op( 700,xfx,'=...').
:- op( 700,xfx,\=).
:- op(1200,xfx,?-).

%% Next operators added by GPS
:- op(650,xfx,'@').
:- op(700,xfx,'in').
:- op(800,xfx,'<-').
:- op(800,fx,'if').
:- op(700,xfx,'^<').
:- op(700,xfx,'^<=').
:- op(700,xfx,'^>').
:- op(700,xfx,'^>=').
:- op(700,xfx,'^=').
:- op(700,xfx,'^\\=').
:- op(1000,fx,(not)).
:- op(900,fx,(rmin)).
:- op(900,fx,(rmax)).
%% Next added by PBC
:- op( 960,xfx,'else').
:- op( 950,xfx,'then').

:- op(1150, fx,(lib)).
