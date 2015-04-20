advisor:what_to_do_today( _today, _weather, _program ):-
    advisor:kind_of_day( _today, _daykind ),
    advisor:kind_of_weather( _weather, _weatherkind ),
    advisor:proposal( _daykind, _weatherkind, _program ).
    

advisor:kind_of_day( monday, workday ). 
advisor:kind_of_day( thuesday, workday ).
advisor:kind_of_day( wednesday, workday ).
advisor:kind_of_day( thursday, workday ).
advisor:kind_of_day( friday, workday ).
advisor:kind_of_day( saturday, weekend ).
advisor:kind_of_day( sunday, weekend ).
advisor:kind_of_day( eastern, feastday ).
advisor:kind_of_day( first_of_may, feastday ).
advisor:kind_of_day( christmas, feastday ).
advisor:kind_of_day( new_years_day, badday ).
advisor:kind_of_day( friday_the_13th, badday ).


advisor:kind_of_weather( sunny, nice ).
advisor:kind_of_weather( rainy, nasty ).
advisor:kind_of_weather( foggy, nasty ).
advisor:kind_of_weather( windy, nasty ).


advisor:proposal( workday, _, go_to_work ).
advisor:proposal( weekend, nice, go_out_to_the_nature ).
advisor:proposal( weekend, nice, visit_the_golf_club ).
advisor:proposal( weekend, nice, wash_your_car ).
advisor:proposal( weekend, nasty, go_out_to_the_town ).
advisor:proposal( weekend, nasty, visit_the_bridge_club ).
advisor:proposal( weekend, nasty, enjoy_yourself_at_home ).
advisor:proposal( weekend, _, it_is_fun_to_learn_Japanese ).
advisor:proposal( badday, _, you_had_better_stay_in_bed ).
advisor:proposal( feastday, _weather, _program ):-
    advisor:proposal( weekend, _weather, _program ).

