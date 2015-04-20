persistent_dir(demo:db,'./db',default,default).
'$is_persistent'(balance/2,demo:db).
calc_interest :- 
          start_transaction(_4394), 
        ( 
          calc_interest(_4394)
         -> 
          commit(_4394)
        ; 
          abort(_4394), 
          rollback(_4394), 
          pause(1), 
          calc_interest
        ).
deposit(_4391,_4392) :- 
          start_transaction(_4397), 
        ( 
          deposit(_4391,_4392,_4397)
         -> 
          commit(_4397)
        ; 
          abort(_4397), 
          rollback(_4397), 
          pause(1), 
          deposit(_4391,_4392)
        ).
calc_interest(_4391) :- 
          (read_lock(balance/2,_4391)->true;!,fail),balance(_4417,_4418),read(_4391,balance(_4417,_4418)), 
          pause(5), 
          _4434 is 0.1*_4418+_4418, 
          (write_lock(balance/2,_4391)->true;!,fail),retract_fact(balance(_4417,_4474)),write(_4391,retract_fact(balance(_4417,_4474))), 
          (write_lock(balance/2,_4391)->true;!,fail),asserta_fact(balance(_4417,_4434)),write(_4391,asserta_fact(balance(_4417,_4434))), 
          fail
.
calc_interest(_4391).
deposit(_4391,_4392,_4393) :- 
          (read_lock(balance/2,_4393)->true;!,fail),balance(_4391,_4420),read(_4393,balance(_4391,_4420)), 
          _4431 is _4420+_4392, 
          (write_lock(balance/2,_4393)->true;!,fail),retract_fact(balance(_4391,_4464)),write(_4393,retract_fact(balance(_4391,_4464))), 
          write_lock(balance/2,_4393)->true;!,fail, 
          asserta_fact(balance(_4391,_4431)), 
          write(_4393,asserta_fact(balance(_4391,_4431)))
.
transactional_demo :- 
          initialize_accounts, 
          display_string([67,111,110,99,117,114,114,101,110,116,108,121,32,99,97,108,99,117,108,97,116,105,110,103,32,49,48,37,32,105,110,116,101,114,101,115,116,32,97,110,100]), 
          nl, 
          display_string([100,101,112,111,115,105,116,105,110,103,32,36,53,48,48,48,32,111,110,32,116,111,112,32,111,102,32,106,111,104,110,39,115,32,98,97,108,97,110,99,101]), 
          nl, 
          display_string([111,102,58,32,36]), 
          balance(john,_4593), 
          display(_4593), 
          nl, 
          display_string([82,101,115,117,108,116,97,110,116,32,98,97,108,97,110,99,101,32,115,104,111,117,108,100,32,98,101,32,36]), 
          _4669 is _4593*0.1+_4593+5000, 
          display(_4669), 
          display_string([32,111,114,32,36]), 
          _4708 is(_4593+5000)*0.1+_4593+5000, 
          display(_4708), 
          nl, 
          display_string([80,108,101,97,115,101,32,87,97,105,116,46,46,46]), 
          nl, 
          two_calls(calc_interest,deposit(john,5000)), 
          display_string([68,111,110,101,32,45,32,82,101,115,117,108,116,32,105,115,58,32,36]), 
          balance(john,_4827), 
          display(_4827), 
          nl
.
non_trans_calc_interest :- 
          balance(_4394,_4395), 
          pause(5), 
          _4405 is 0.1*_4395+_4395, 
          retract_fact(balance(_4394,_4424)), 
          asserta_fact(balance(_4394,_4405)), 
          fail
.
non_trans_calc_interest.
non_trans_deposit(_4391,_4392) :- 
          balance(_4391,_4398), 
          _4403 is _4398+_4392, 
          retract_fact(balance(_4391,_4415)), 
          asserta_fact(balance(_4391,_4403))
.
non_transactional_demo :- 
          initialize_accounts, 
          display_string([67,111,110,99,117,114,114,101,110,116,108,121,32,99,97,108,99,117,108,97,116,105,110,103,32,49,48,37,32,105,110,116,101,114,101,115,116,32,97,110,100]), 
          nl, 
          display_string([100,101,112,111,115,105,116,105,110,103,32,36,53,48,48,48,32,111,110,32,116,111,112,32,111,102,32,106,111,104,110,39,115,32,98,97,108,97,110,99,101]), 
          nl, 
          display_string([111,102,58,32,36]), 
          balance(john,_4593), 
          display(_4593), 
          nl, 
          display_string([82,101,115,117,108,116,97,110,116,32,98,97,108,97,110,99,101,32,115,104,111,117,108,100,32,98,101,32,36]), 
          _4669 is _4593*0.1+_4593+5000, 
          display(_4669), 
          display_string([32,111,114,32,36]), 
          _4708 is(_4593+5000)*0.1+_4593+5000, 
          display(_4708), 
          nl, 
          display_string([80,108,101,97,115,101,32,87,97,105,116,46,46,46]), 
          nl, 
          two_calls(non_trans_calc_interest,non_trans_deposit(john,5000)), 
          display_string([68,111,110,101,32,45,32,82,101,115,117,108,116,32,105,115,58,32,36]), 
          balance(john,_4827), 
          display(_4827), 
          nl, 
          display_string([79,98,118,105,111,117,115,108,121,32,116,114,97,110,115,97,99,116,105,111,110,115,32,97,114,101,32,110,101,99,101,115,115,97,114,121,33]), 
          nl
.
two_calls(_4391,_4392) :- 
          eng_call(_4391,create,create,_4400), 
          pause(2), 
          eng_call(_4392,create,create,_4413), 
          eng_wait(_4400), 
          eng_release(_4400), 
          eng_wait(_4413), 
          eng_release(_4413)
.
initialize_accounts :- 
          balance(john,_4401)->retract_fact(balance(john,_4406));true, 
          balance(peter,_4418)->retract_fact(balance(peter,_4423));true, 
          asserta_fact(balance(john,8000)), 
          asserta_fact(balance(peter,12000))
.
