

:- module(ssuply, [ssupply/4]).
:- ensure_loaded(local).

ssupply( _snum, _pnum, _qty, trace(1, L, B, S) ) :-
    london_red_heavy_parts( _pnum, _pname, _weight, L ),
    best_suppliers( _snum, _sname, _status, _city, B ),
    supply( _snum, _pnum, _qty, S ).

best_suppliers( _snum, _sname, _status, _scity, trace(2, GS, GC, '=') ) :-
    good_suppliers( _snum, _sname, _status, _scity, GS ),
    good_cities( _city, GC ),
    _city = _scity.

good_suppliers( _snum, _sname, _status, _city, trace(3, S, G) ) :-
    suppliers( _snum, _sname, _status, _city, S ),
    local:'>'(_status, 10, G).

london_red_heavy_parts( _pnum, _pname, _weight, trace(4, LR, LH, '=') ) :-
    london_red_parts( _pnum, _pname, _weight, LR ),
    london_heavy_weights( _pno, _pname, _colour, _weight, LH ),
    _pnum = _pno.

london_red_parts( _pnum, _pname, _weight, trace(5, R, '=') ) :-
    red_parts( _pnum, _pname, _weight, _city, R ),
    _city = london.

red_parts( _pnum, _pname, _weight, _city, trace(6, P, '=') ) :-
    parts( _pnum, _pname, _colour, _weight, _city, P ),
    _colour = red.

london_heavy_weights( _pnum, _pname, _colour, _weight, trace(7, H, '=') ) :-
    heavy_weights( _pnum, _pname, _colour, _weight, _city, H ),
    _city = london .

heavy_weights( _pnum, _pname, _colour, _weight, _city, trace(8, P, G) ) :-
    parts( _pnum, _pname, _colour, _weight, _city, P ), 
    local:'>'(_weight, 10, G).

good_cities( paris, 9 ).
good_cities( london, 10 ).
good_cities( hongkong, 11 ).
good_cities( regina, 12 ).
good_cities( saskatoon, 13 ).

supply( s1, p1, 300, 14 ).
supply( s1, p2, 200, 15 ).
supply( s1, p3, 400, 16 ).
supply( s2, p1, 300, 17 ).
supply( s2, p2, 400, 18 ).
supply( s3, p1, 400, 19 ).
supply( s4, p1, 200, 20 ).
supply( s5, p1, 500, 21 ).
supply( s5, p2, 400, 22 ).

parts( p1, nut, red, 12, london, 23 ).
parts( p2, bolt, green, 17, paris, 24 ).
parts( p3, screw, blue, 17, rome, 25 ).

suppliers( s1, smith, 20, london, 26 ).
suppliers( s2, jones, 10, paris, 27 ).
suppliers( s3, blake, 30, paris, 28 ).
suppliers( s4, clark, 20, london, 29 ).
suppliers( s5, adams, 30, athens, 30 ).

