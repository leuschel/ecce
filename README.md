# ecce
Online partial evaluator for pure Prolog programs (with built-ins)

ECCE is an automatic partial deduction system for logic programs.
It takes a pure Prolog program and a query of interest and then specialises the program for that particular query. 

Implemented by Michael Leuschel
Based on work by Michael Leuschel, Bern Martens, Jesper Jorgensen,
Danny De Schreye, Robert Glueck, Morten Heine Sorensen, Andre de Waal,
and Mauricio Varea.
(C) 1995-2015

An online version of Ecce is available at:
  http://wyvern.cs.uni-duesseldorf.de/ecce/index.php

To build the command-line version of ecce (ecce_cli) simply execute make in the
main directory or within the ecce_source directory. This will also run a simple test.
You need SICStus Prolog 4.3 or later to generate the Ecce binary.