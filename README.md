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

## Pointers to the Research

The implementation of the ECCE system is mainly based on the following research papers:

- Ecological Partial Deduction: Preserving Characteristic Trees Without Constraints. Michael Leuschel.
Global Control for Partial Deduction through Characteristic Atoms and Global Trees. Michael Leuschel and Bern Martens.
- Controlling Generalisation and Polyvariance in Partial Deduction of Normal Logic Programs. Michael Leuschel, Bern Martens, and Danny De Schreye. ACM Transactions on Programming Languages and Systems (Toplas), volume 20(1), pages 208-258.
- A Conceptual Embedding of Folding into Partial Deduction: Towards a Maximal Integration. Michael Leuschel, Danny De Schreye, and Andre de Waal.
- Controlling Conjunctive Partial Deduction of Definite Logic Programs. Robert Glück, Jesper Jørgensen, Bern Martens and Morten H. Sørensen.
- Redundant Argument Filtering of Logic Programs. Michael Leuschel and Morten H. Sorensen
- Conjunctive Partial Deduction: Foundations, Control, Algorithms, and Experiments. Danny De Schreye, Robert Glück, Jesper Jørgensen, Michael Leuschel, Bern Martens and Morten Heine Sørensen The Journal of Logic Programming 41, pages 231-277. November 1999.

The following paper describes experiments done with the system on the DPPD library of benchmarks:
- Conjunctive Partial Deduction in Practice. Jesper Jørgensen, Michael Leuschel, and Bern Martens
- Advanced Techniques for Logic Program Specialisation. Michael Leuschel. PhD Thesis.
