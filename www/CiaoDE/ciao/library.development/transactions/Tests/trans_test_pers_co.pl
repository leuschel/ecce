persistent_dir(trans_test_pers:db_test,./,default,default).
'$is_persistent'(balance/2,trans_test_pers:db_test).
calc_interest :- 
          start_transaction(_2885), 
        ( 
          calc_interest(_2885)
         -> 
          commit(_2885)
        ; 
          abort(_2885), 
          rollback(_2885), 
          pause(1), 
          calc_interest
        ).
deposit(_2882,_2883) :- 
          start_transaction(_2888), 
        ( 
          deposit(_2882,_2883,_2888)
         -> 
          commit(_2888)
        ; 
          abort(_2888), 
          rollback(_2888), 
          pause(1), 
          deposit(_2882,_2883)
        ).
calc_interest(_2882) :- 
          (read_lock(balance/2,_2882)->true;!,fail),balance(_2908,_2909),read(_2882,balance(_2908,_2909)), 
          pause(5), 
          _2925 is 0.1*_2909+_2909, 
          (write_lock(balance/2,_2882)->true;!,fail),retract_fact(balance(_2908,_2965)),write(_2882,retract_fact(balance(_2908,_2965))), 
          (write_lock(balance/2,_2882)->true;!,fail),asserta_fact(balance(_2908,_2925)),write(_2882,asserta_fact(balance(_2908,_2925))), 
          fail
.
calc_interest(_2882).
deposit(_2882,_2883,_2884) :- 
          (read_lock(balance/2,_2884)->true;!,fail),balance(_2882,_2911),read(_2884,balance(_2882,_2911)), 
          pause(5), 
          _2927 is _2911+_2883, 
          (write_lock(balance/2,_2884)->true;!,fail),retract_fact(balance(_2882,_2960)),write(_2884,retract_fact(balance(_2882,_2960))), 
          write_lock(balance/2,_2884)->true;!,fail, 
          asserta_fact(balance(_2882,_2927)), 
          write(_2884,asserta_fact(balance(_2882,_2927)))
.
three_calls(_2882,_2883,_2884) :- 
          eng_call(_2882,create,create,_2892), 
          pause(2), 
          eng_call(_2883,create,create,_2905), 
          pause(3), 
          eng_call(_2884,create,create,_2918), 
          eng_wait(_2892), 
          eng_release(_2892), 
          eng_wait(_2905), 
          eng_release(_2905), 
          eng_wait(_2918), 
          eng_release(_2918)
.
initialize :- 
          retract_fact(_2885), 
          fail
.
initialize.
demo_txns :- 
          initialize, 
          asserta_fact(balance(john,8000)), 
          asserta_fact(balance(peter,12000)), 
          asserta_fact(balance(jorge,1000)), 
          display_string([67,111,110,99,117,114,114,101,110,116,108,121,32,100,101,112,111,115,105,116,105,110,103,32,36,49,48,48,48,48,32,111,110,32,116,111,112,32,111,102,32,106,111,114,103,101,39,115,32,98,97,108,97,110,99,101,32]), 
          nl, 
          display_string([100,101,112,111,115,105,116,105,110,103,32,36,53,48,48,48,32,111,110,32,116,111,112,32,111,102,32,106,111,104,110,39,115,32,98,97,108,97,110,99,101]), 
          nl, 
          display_string([100,101,112,111,115,105,116,105,110,103,32,36,51,48,48,48,32,111,110,32,116,111,112,32,111,102,32,112,101,116,101,114,39,115,32,98,97,108,97,110,99,101]), 
          nl, 
          display_string([82,101,115,117,108,116,97,110,116,32,98,97,108,97,110,99,101,32,115,104,111,117,108,100,32,98,101,32,36]), 
          display_string([106,111,114,103,101,39,115,32,98,97,108,97,110,99,101,32,111,102,58,32,36]), 
          balance(jorge,_3327), 
          display(_3327), 
          nl, 
          _3340 is _3327+10000, 
          display(_3340), 
          nl, 
          display_string([112,101,116,101,114,39,115,32,98,97,108,97,110,99,101,32,111,102,58,32,36]), 
          balance(peter,_3405), 
          display(_3405), 
          nl, 
          _3418 is _3405+3000, 
          display(_3418), 
          nl, 
          display_string([106,111,104,110,39,115,32,98,97,108,97,110,99,101,32,111,102,58,32,36]), 
          balance(peter,_3481), 
          display(_3481), 
          nl, 
          _3494 is _3481+5000, 
          display(_3494), 
          nl, 
          three_calls(deposit(jorge,10000),deposit(john,5000),deposit(peter,3000)), 
          display_string([68,111,110,101,32,45,32,106,111,114,103,101,39,115,32,98,97,108,97,110,99,101,58,32,36]), 
          balance(jorge,_3583), 
          display(_3583), 
          nl, 
          display_string([68,111,110,101,32,45,32,112,101,116,101,114,39,115,32,98,97,108,97,110,99,101,58,32,36]), 
          balance(peter,_3652), 
          display(_3652), 
          nl, 
          display_string([68,111,110,101,32,45,32,106,111,104,110,39,115,32,98,97,108,97,110,99,101,58,32,36]), 
          balance(john,_3719), 
          display(_3719), 
          nl
.
