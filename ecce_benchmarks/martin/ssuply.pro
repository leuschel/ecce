/* file: ssupply.pro */

ssuply:ssupply( _snum, _pnum, _qty ) :-
    ssuply:london_red_heavy_parts( _pnum, _pname, _weight ),
    ssuply:best_suppliers( _snum, _sname, _status, _city ),
    ssuply:supply( _snum, _pnum, _qty ).

ssuply:best_suppliers( _snum, _sname, _status, _scity ) :-
    ssuply:good_suppliers( _snum, _sname, _status, _scity ),
    ssuply:good_cities( _city ),
    _city = _scity .

ssuply:good_suppliers( _snum, _sname, _status, _city ) :-
    ssuply:suppliers( _snum, _sname, _status, _city ),
    _status > 10 .

ssuply:london_red_heavy_parts( _pnum, _pname, _weight ) :-
    ssuply:london_red_parts( _pnum, _pname, _weight ),
    ssuply:london_heavy_weights( _pno, _pname, _colour, _weight ),
    _pnum = _pno .

ssuply:london_red_parts( _pnum, _pname, _weight ) :-
    ssuply:red_parts( _pnum, _pname, _weight, _city ),
    _city = london .

ssuply:red_parts( _pnum, _pname, _weight, _city ) :-
    ssuply:parts( _pnum, _pname, _colour, _weight, _city ),
    _colour = red .

ssuply:london_heavy_weights( _pnum, _pname, _colour, _weight ) :-
    ssuply:heavy_weights( _pnum, _pname, _colour, _weight, _city ),
    _city = london .

ssuply:heavy_weights( _pnum, _pname, _colour, _weight, _city ) :-
    ssuply:parts( _pnum, _pname, _colour, _weight, _city ), 
    _weight > 10 .

ssuply:good_cities( paris ).
ssuply:good_cities( london ).
ssuply:good_cities( hongkong ).
ssuply:good_cities( regina ).
ssuply:good_cities( saskatoon ).

ssuply:supply( s1, p1, 300 ).
ssuply:supply( s1, p2, 200 ).
ssuply:supply( s1, p3, 400 ).
ssuply:supply( s2, p1, 300 ).
ssuply:supply( s2, p2, 400 ).
ssuply:supply( s3, p1, 400 ).
ssuply:supply( s4, p1, 200 ).
ssuply:supply( s5, p1, 500 ).
ssuply:supply( s5, p2, 400 ).

ssuply:parts( p1, nut, red, 12, london ).
ssuply:parts( p2, bolt, green, 17, paris ).
ssuply:parts( p3, screw, blue, 17, rome ).

ssuply:suppliers( s1, smith, 20, london ).
ssuply:suppliers( s2, jones, 10, paris ).
ssuply:suppliers( s3, blake, 30, paris ).
ssuply:suppliers( s4, clark, 20, london ).
ssuply:suppliers( s5, adams, 30, athens ).


