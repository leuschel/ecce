<html lang="en">
<head>
  <link rel="stylesheet" type="text/css" href="../mecce.css"/>
  <title>Help on ECCE</title>
</head>

<body>
<center>
<img src="../ecce.jpg" width="300"/>
</center>

<div style="width:670px">
<p><code>ECCE</code> is an automatic online program specialiser for pure Prolog programs
 (with built-ins).
It takes a pure Prolog program and a query of interest and then specialises
the program for that particular query.
The research behind the <code>ECCE</code> system is described, amongst others, in the following
 papers:
<ul>
 <li><a href="http://eprints.ecs.soton.ac.uk/524/" target="_blank">ACM Transactions on
   Programming Languages and Systems 20(1), pp. 208-258.</a></li>
 <li><a href="http://eprints.ecs.soton.ac.uk/489/" target="_blank">Journal Logic Programming
  41(2&3):pp. 231-277.</a></li>
</ul>
More information about how partial deduction is controlled
 can be found in:
<ul>
 <li><a href="http://eprints.ecs.soton.ac.uk/9186/" target="_blank">Theory and Practice of
 Logic Programming 2805(4&5):pp. 461-515.</a></li>
</ul></p>
<br/>

<h1>TUTORIAL</h1>

<img style="float:right; margin:0 10px;" src="images/upload_crop.jpg" width="430"/>
<p>There are three ways in which the user can input some <i>source</i> code to <code>ECCE</code>: 
<ol>
<li>choosing a file from disk, and loading it, or</li> 
<li>selecting a pre-loaded working example from the right-hand drop menu, or</li>
<li>pasting straight into the <i>source</i> text area.</li>
</ol>
The source code must be a pure Prolog program, which can contain built-ins, and this is going to be used as input to the specialiser.</p>

<img style="float:left; margin:0 10px;" src="images/actions.jpg" width="200"/>
<p>There are five <i>actions</i> available for this input code: Specialisation, Slicing, Most Specific Version (MSV) bottom-up propagation, Redundant Argument Filtering (RAF), and inverse Redundant Argument Filtering (FAR).</p>

<!-- 
<img style="float:right; margin:0 10px;" src="images/plugins.jpg" width="150"/>
<p>In addition to these actions, the user can also perform some extra analysis given in the <i>plug-ins</i> section.
Currently, there are two plug-ins incorporated to the tool: Regular Unary Logic (RUL) program analisys and RUL bottom-up analysis.   
</p>
 -->

<img style="float:left; margin:0 10px;" src="images/goal.jpg" width="200"/>
<p>Actions that are not bottom-up, also require the user to type in a goal in the appropriate text box.
These are: Specialisation, Slicing, and the RAF  analysis.</p> <!-- and RUL -->

<p>The user can perform the partial evaluation, i.e., specialisation, of a given source program for a given goal.
For instance, by partial evaluation of the following Prolog code (loaded as source, as explained before):
<pre class="console">
doubleapp(X,Y,Z,XYZ) :- append(X,Y,XY), append(XY,Z,XYZ).
append([],L,L).
append([H|X],Y,[H|Z]) :- append(X,Y,Z).
rev([],X,X).
rev([H|X],A,R) :- rev(X,[H|A],R).
p :- q.
q.
</pre>
using the following goal:
<pre class="console">
doubleapp(X,Y,Z,XYZ)
</pre>
should produce the following specialised code:
<pre class="console">
doubleapp(A,B,C,D) :- 
    doubleapp__1(A,B,C,D).
doubleapp__1([],A,B,C) :- 
    append__3(A,B,C).
doubleapp__1([A|B],C,D,[A|E]) :- 
    append_conj__2(B,C,D,E).
append_conj__2([],A,B,C) :- 
    append__3(A,B,C).
append_conj__2([A|B],C,D,[A|E]) :- 
    append_conj__2(B,C,D,E).
append__3([],A,A).
append__3([A|B],C,[A|D]) :- 
    append__3(B,C,D).
</pre>
Some information about the specialisation process is also included in the header of the resulting file, such as parameters that were used and timing information.</p>


<!-- 

<pre>
solve([]).
solve([A|T]) :- solve_atom(A), solve(T).

solve_atom(A) :- my_clause(A,B), solve(B).

my_clause(app([],L,L),[]).
my_clause(app([H|X],Y,[H|Z]),[app(X,Y,Z)]).
my_clause(p,[p]).
my_clause(solve2([]),[]).
my_clause(solve2([A|T]), [solve_atom2(A), solve2(T)]).
my_clause(solve_atom2(A), [my_clause2(A,B), solve2(B)]).
my_clause(my_clause2(app([],L,L),[]),[]).
my_clause(my_clause2(app([H|X],Y,[H|Z]),[app(X,Y,Z)]),[]).
</pre>

<pre>
solve_atom(solve_atom2(app(A,B,C))) :- 
    solve_atom__1(A,B,C).
solve_atom__1([],A,A).
solve_atom__1([A|B],C,[A|D]) :- 
    solve_atom__1(B,C,D).
</pre>

-->

<p>The slicing algorithm of <code>ECCE</code> (described in <a href="http://eprints.ecs.soton.ac.uk/10797/" target="_blank">Proceedings
 ESOP'05, LNCS 3444:pp. 61-76.</a>) operates in a similar way: program and goal are determined by the user and, as a result of clicking the <code>Slice</code> button, a sliced version of his original program is produced.
For instance, take the &quot;advisor.pl&quot; working example:
<pre class="console">
what_to_do_today( _today, _weather, _program ):-
    kind_of_day( _today, _daykind ),
    kind_of_weather( _weather, _weatherkind ),
    proposal( _daykind, _weatherkind, _program ).
    
kind_of_day( monday, workday ). 
kind_of_day( thuesday, workday ).
kind_of_day( wednesday, workday ).
kind_of_day( thursday, workday ).
kind_of_day( friday, workday ).
kind_of_day( saturday, weekend ).
kind_of_day( sunday, weekend ).
kind_of_day( eastern, feastday ).
kind_of_day( first_of_may, feastday ).
kind_of_day( christmas, feastday ).
kind_of_day( new_years_day, badday ).
kind_of_day( friday_the_13th, badday ).

kind_of_weather( sunny, nice ).
kind_of_weather( rainy, nasty ).
kind_of_weather( foggy, nasty ).
kind_of_weather( windy, nasty ).

proposal( workday, _, go_to_work ).
proposal( weekend, nice, go_out_to_the_nature ).
proposal( weekend, nice, visit_the_golf_club ).
proposal( weekend, nice, wash_your_car ).
proposal( weekend, nasty, go_out_to_the_town ).
proposal( weekend, nasty, visit_the_bridge_club ).
proposal( weekend, nasty, enjoy_yourself_at_home ).
proposal( weekend, _, it_is_fun_to_learn_Japanese ).
proposal( badday, _, you_had_better_stay_in_bed ).
proposal( feastday, _weather, _program ):-
    proposal( weekend, _weather, _program ).
</pre>
and slice it for the following goal:
<pre class="console">
what_to_do_today( first_of_may, W, P )
</pre>
</p>

<p>What you should get back is a program that looks like this:
<div class="console">
<code>
what_to_do_today( _today, _weather, _program ):-<br />
&nbsp;&nbsp;&nbsp;&nbsp;kind_of_day( _today, _daykind ),<br />
&nbsp;&nbsp;&nbsp;&nbsp;kind_of_weather( _weather, _weatherkind ),<br />
&nbsp;&nbsp;&nbsp;&nbsp;proposal( _daykind, _weatherkind, _program ).<br />

<div class="sliced">%kind_of_day( monday, workday ). <br />
</div>
<div class="sliced">%kind_of_day( thuesday, workday ).<br />
</div>
<div class="sliced">%kind_of_day( wednesday, workday ).<br />
</div>
<div class="sliced">%kind_of_day( thursday, workday ).<br />
</div>
<div class="sliced">%kind_of_day( friday, workday ).<br />
</div>
<div class="sliced">%kind_of_day( saturday, weekend ).<br />
</div>
<div class="sliced">%kind_of_day( sunday, weekend ).<br />
</div>
<div class="sliced">%kind_of_day( eastern, feastday ).<br />
</div>
kind_of_day( first_of_may, feastday ).<br />
<div class="sliced">%kind_of_day( christmas, feastday ).<br />
</div>
<div class="sliced">%kind_of_day( new_years_day, badday ).<br />
</div>
<div class="sliced">%kind_of_day( friday_the_13th, badday ).<br />
%<br />
</div>
kind_of_weather( sunny, nice ).<br />
kind_of_weather( rainy, nasty ).<br />
kind_of_weather( foggy, nasty ).<br />
kind_of_weather( windy, nasty ).<br />
<br />
<div class="sliced">%proposal( workday, _, go_to_work ).<br />
</div>
proposal( weekend, nice, go_out_to_the_nature ).<br />
proposal( weekend, nice, visit_the_golf_club ).<br />
proposal( weekend, nice, wash_your_car ).<br />
proposal( weekend, nasty, go_out_to_the_town ).<br />
proposal( weekend, nasty, visit_the_bridge_club ).<br />
proposal( weekend, nasty, enjoy_yourself_at_home ).<br />
proposal( weekend, _, it_is_fun_to_learn_Japanese ).<br />
<div class="sliced">%proposal( badday, _, you_had_better_stay_in_bed ).<br />
</div>
proposal( feastday, _weather, _program ):-<br />
&nbsp;&nbsp;&nbsp;&nbsp;proposal( weekend, _weather, _program ).
</code>
</div>
</p>



<h2>The Control Settings</h2>

The control settings influence the global control
(the control of polyvariance, i.e., how many specialised
predicates are generated) and local control
(the unfolding or inlining, i.e., how much does the specialiser
 pre-compute)
of the specialiser.

<ol>
 <li> <b>Default Conjunctive</b>:
   This uses conjunctive partial deduction (i.e., multiple calls
    can be specialised in conjunction; in other words a single
    specialised predicate can represent a conjunction of calls)
    with the default control settings. The local control is not
    very aggressive, it basically uses determinacy with a lookahead
    of 1. For global control characteristic trees are used.
     </li>
 <li> <b>Default Fast</b>: this is a less aggressive version of the
 above. Use it if the above setting times out or gives you too big
 a specialised program.
     </li>
 
 <li> <b>Classic</b>:
  This uses classical partial deduction (i.e., only a single call
  can be specialised at a time; there are no conjunctions at the
  global control level) with the default settings for this.
  The unfolding is more aggressive than for default conjunctive;
   the global control uses characteristic trees.
     </li>
 <li> <b>Mixtus</b>: This uses classical partial deduction, but tries
  to mimic the control employed by the Mixtus specialiser (but it is
  not a 100 percent faithful emulation).
     </li>
 <li> <b>Classic-Fast</b>: this is a less aggressive version of the classic setting
 above. Use it if the classic setting times out or gives you too big
 a specialised program.
     </li>
 <li> <b>Minimal</b>: this is a very conservative classical 
 partial deduction: it uses determinate unfolding but is very conservative as far as the generalisation
 is concerned (will generalize if a new specialized predicate is not
 more general than all the preceding ones).
     </li>
     
 <li> <b>Termination</b>: this is a version of Classic which ensures
  that termination characteristics are not changed (not yet fully tested;
   please report if termination characteristics are changed).
   Useful if you want to use Ecce as a pre-processor for a termination
   analyser. Be sure to leave the post-processor options as off or default.
     </li>
     
</ol>

<h2>The Post Processing Settings</h2>

The control settings influence the post processor.


<ol>
 <li> <b>Max</b>:
   All post-processing is turned on:
     determinate post-unfolding, RAF+FAR redundant argument filtering,
     dead code elimination, removal of duplicate calls,
      polyvariance minimization,
      Most Specific Version computation,...
     </li>
     
 <li> <b>Default</b>: The same as above, but the Most Specific Version
  computation is turned off.
     </li>
 <li> <b>Off</b>: All post-processing is turned off.
     </li>
</ol>
     

<h2>The Specialisation Tree</h2>
<?php
include "svg.html"
?>

</div> <!-- fixed width: 670 -->

</body>
</html>