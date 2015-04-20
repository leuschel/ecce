:- use_module(library('chartlib/genbar1')).
:- use_module(library('chartlib/genbar2')).

main :-
	barchart1('This is the header text',
          'Barchart title',
          'xaxistitle',
          [ ['element1','pr1','Blue','Yellow','pattern1'],
                 ['element2','pr2','MediumTurquoise','Plum','pattern5'],
                 ['element3','pr3','MediumTurquoise','Green','pattern5'] ],
          'yaxixtitle',
          [20,10,59],
          'footer').
