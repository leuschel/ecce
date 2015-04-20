#!/bin/sh
rm *.po *.itf
export CIAOLIB=/home/clip/Systems/ciao-1.9 
export CIAOENGINE=/home/clip/Systems/ciao-1.9/bin/LINUXi86/ciaoengine
/home/clip/Systems/ciao-1.9/ciaoc/ciaoc -x a.pl
CHOICESTKSIZE=4095 ./a

