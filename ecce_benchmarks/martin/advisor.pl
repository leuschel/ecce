:- module(advisor, [what_to_do_today/4]).
:- ensure_loaded(local).

what_to_do_today( _today, _weather, _program, trace(1, D, W, P) ):-
    kind_of_day( _today, _daykind, D ),
    kind_of_weather( _weather, _weatherkind, W ),
    proposal( _daykind, _weatherkind, _program, P ).

kind_of_day( monday, workday, 2 ). 
kind_of_day( thuesday, workday, 3 ).
kind_of_day( wednesday, workday, 4 ).
kind_of_day( thursday, workday, 5 ).
kind_of_day( friday, workday, 6 ).
kind_of_day( saturday, weekend, 7 ).
kind_of_day( sunday, weekend, 8 ).
kind_of_day( eastern, feastday, 9 ).
kind_of_day( first_of_may, feastday, 10 ).
kind_of_day( christmas, feastday, 11 ).
kind_of_day( new_years_day, badday, 12 ).
kind_of_day( friday_the_13th, badday, 13 ).

kind_of_weather( sunny, nice, 14 ).
kind_of_weather( rainy, nasty, 15 ).
kind_of_weather( foggy, nasty, 16 ).
kind_of_weather( windy, nasty, 17 ).

proposal( workday, _, go_to_work, 18 ).
proposal( weekend, nice, go_out_to_the_nature, 19 ).
proposal( weekend, nice, visit_the_golf_club, 20 ).
proposal( weekend, nice, wash_your_car, 21 ).
proposal( weekend, nasty, go_out_to_the_town, 22 ).
proposal( weekend, nasty, visit_the_bridge_club, 23 ).
proposal( weekend, nasty, enjoy_yourself_at_home, 24 ).
proposal( weekend, _, it_is_fun_to_learn_Japanese, 25 ).
proposal( badday, _, you_had_better_stay_in_bed, 26 ).
proposal( feastday, _weather, _program, trace(27, P) ) :-
    proposal( weekend, _weather, _program, P ).
