:- module(dictionary_tree, [
	create_dictionaries/1,
	is_dictionaries/1,
	get_definition_dictionary/2,
	get_prototype_dictionary/2,
	dictionary_insert/5,
	dictionary_lookup/5,
	merge_tree/2
	],
	[assertions,basicmodes,regtypes]).

:- include(library(iso)).
:- use_module(library(iso_byte_char)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(internal_types).

:- set_prolog_flag(multi_arity_warnings, off).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(module,"This module offers a dynamic tree structured dictionary
                   a bit combined with predicates that gives it the useability
                   to be the dictionary for the parser.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_dictionaries(-Dictionary)
   :: dictionary
    # "Returns a dictionary. A general name was used if the user would like
       to  change the code to include more dictionaries.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_dictionaries(dic(Proto)) :-
	create_tree(Proto).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred is_dictionaries(?Dictionary)
   :: dictionary
    # "Is the argument a dictionary is solved by this predicate.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_dictionaries(dic(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_tree(-Tree)
	:: tree
        # "Creates an empty tree structure used as the base structure for a 
           dictionary.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tree(tree(_Key,_Value,_LeftTree,_RightTree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_tree(+Element,-Tree)
	:: element * tree
        # "From an element construct a tree structure using the values 
           of the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tree(Element,tree(Key,Leaf, _Left, _Right)) :-
	get_element_key(Element,Key),
	create_leaf(Element,Leaf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred create_leaf(+Element,-Leaf)
	:: element * leaf
        # "Creates a leaf structure from a given element. The leaf is the 
           information post in the tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_leaf(element(Key,Type,Dic),leaf(Key,Type,Dic,_MoreLeafs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_definition_dictionary(+Dictionary,-Tree)
   :: dictionary * tree
    # "Returns the definition dictionary (for the moment there is only one
       dictionary), which is a tree representation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_definition_dictionary(dic(D),D).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_prototype_dictionary(+Dictionary,-Tree)
   :: dictionary * tree
    # "Returns the prototype dictionary (for the moment there is only one
       dictionary), which is a tree representation.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_prototype_dictionary(dic(P),P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_left_tree(+Tree,-Left_tree)
	:: tree * tree
        # "The predicate will return the left search tree of the input tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_left_tree(tree(_Key,_Value,Left,_Right),Left).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_right_tree(+Tree,-Right_tree)
	:: tree * tree
        # "The predicate will return the left search tree of the input tree.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_right_tree(tree(_Key,_Value,_Left,Right),Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_tree_key(+Tree, -Key)
	:: tree * atm
        # "The predicate vill return the key value at the current position.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tree_key(tree(Key,_Value,_Left,_Right),Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_tree_leaf(+Tree,-Leaf)
	:: tree * leaf
        # "The predicate returns the leaf value at the position, 
           that is the information that is set with the current key value.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_tree_leaf(tree(_Key,Leaf,_Left,_Right),Leaf).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_information(+Leaf,-Info)
	:: leaf * term
        # "The predicate will return the information that is within the
           leaf. The information is what the user has placed as information
           and can be any type of term.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_information(leaf(_Key, _Type, Info,_MoreLeafs),Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_type(+Leaf,-Type)
	:: leaf * atm
        # "From a leaf structure will return the type, a user elected 
           template to identify different types of information inserts.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_type(leaf(_Key,Type,_Info,_MoreLeafs),Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_rest(+Leaf,-Rest_of_leafs)
	:: leaf * leaf
        # "The leaf for a given key will contain not only one value, 
           but can contain more values with the same key but of different 
           properties. These extra values will be placed dynamically in the 
           open body of the leaf. So from a given leaf will return the 
           possible descendents with the same key value. If no leaf is bound 
           a variable is returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_rest(leaf(_Key,_Type,_Info,MoreLeafs),MoreLeafs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_leaf_key(+Leaf,-Key)
	:: leaf * atm
        # "From a leaf structure will return the key value for the leaf.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_leaf_key(leaf(Key,_Type,_Info,_More),Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_last_leaf(+Leaf,-Last_leaf)
	:: leaf * leaf
        # "From a leaf returns the last leaf with the same key value.
           If it is the last leaf then the input will be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_last_leaf(Leaf,Last_leaf) :-
	(  nonvar(Leaf)
	-> get_leaf_rest(Leaf,Rest),
	   ( nonvar(Rest)
	   ->get_last_leaf(Rest,Last_leaf)
	   ; Last_leaf = Rest
	   )
	;  Last_leaf = Leaf
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_next_leaf(+Leaf, -Next_leaf) 
	:: leaf * term
        # "The predicate will return the next leaf with the same key value.
           If the are no more leafs the atom last will be returned.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_next_leaf(Leaf,Next) :-
	get_leaf_rest(Leaf,Rest),
	(var(Rest)
	->
	Next = last
	;
	Next = Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred element_not_on_branch(+Element,+Leaf)
	:: element * leaf
        # "The predicate will check if the element not exists in the leaf
	   descendents, that is as the elements with the same key value. You
           are allowed to insert post that differs a bit, but we do not want 
           duplicates.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

element_not_on_branch(E,Leaf) :-
	( compare_element_leaf(E,Leaf)
	-> fail
	; get_next_leaf(Leaf,Next),
	  ( Next = last
	  ->true
	  ; element_not_on_branch(Next,E)
	  )
	).
	   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred compare_element_leaf(+Element,+Leaf)
	:: element * leaf
        # "The predicate will compare the element values against those
           of a leaf.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_element_leaf(Element,Leaf) :-
	get_leaf_key(Leaf,Key),
	get_element_key(Element,Key),
	get_leaf_type(Leaf,Type),
	get_element_type(Element,Type),
	get_leaf_information(Leaf,Info),
	get_element_dic(Element,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred is_tree(+Tree) 
	:: tree
        # "The argument is a tree structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_tree(tree(K,V,_,_)) :-
	nonvar(K),
	nonvar(V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred is_leaf(+Leaf)
	:: leaf
        # "The predicate is a leaf structure.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_leaf(Leaf) :-
	nonvar(Leaf),
	Leaf = leaf(_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred got_left_tree(+Tree)
	:: tree
        # "The tree have a left tree instance.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
got_left_tree(Tree) :-
	is_tree(Tree),
	get_left_tree(Tree,B),
	nonvar(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred got_right_tree(+Tree)
	:: tree
        # "The tree have a right tree instance.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

got_right_tree(Tree) :-
	is_tree(Tree),
	get_right_tree(Tree,B),
	nonvar(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_element_key(+Element,-Key)
	:: element * atm
        # "From an element structure will return the search key for the 
           element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_element_key(element(Key,_Type,_Dic),Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_element_type(+Element,-Type)
	:: element * atm
        # "Returns the element type for the given element, user specified.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_element_type(element(_Key,Type,_Dic),Type).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred get_element_dic(+Element,-Dictionary)
   :: element * dictionary
   # "Returns the ditionary type for the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_element_dic(element(_Key,_Type,Dic),Dic).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_element_keyname(element(Key,_Type,_Dic),Keyname) :-
	name(Keyname,Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_branch(Key,Value,left) :-
	compare_key_lt(Key,Value),
	!.

choose_branch(Key,Value,right) :-
	compare_key_gt(Key,Value).

select_branch(left,Tree,Left) :-
	get_left_tree(Tree,Left).

select_branch(right,Tree,Right) :-
	get_right_tree(Tree,Right).

change_direction(Key,Tree,Branch) :-
	get_tree_key(Tree,Tree_key),
	choose_branch(Key,Tree_key,Branch_choise),
	select_branch(Branch_choise,Tree,Branch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compare_key_eq(K0,K1) :-
	compare_list_eq(K0,K1).

compare_key_leq(K0,K1) :-
	compare_list_leq(K0,K1).

compare_key_lt(K0,K1) :-
	compare_list_lt(K0,K1).

compare_key_gt(K0,K1) :-
	compare_list_gt(K0,K1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_seek(+Key,+Type,+Post_in,+Dictionary,-Post_out)
	:: atm * atm * term * dictionary * term
        # "This predicate is used to find the right place for a post
           and return the apropiate leaf structure to use for the 
           insert place.".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_seek(Name,Type,Field,Dictionary,Info) :-
	%name(Name, Key),
	dictionary_seek(element(Name,Type,Field),Dictionary,Leaf),
	get_last_leaf(Leaf,Last),
	get_leaf_information(Last,Info).

dictionary_seek(element(Key,Type,Info),Tree,Leaf) :-
	climb_tree(element(Key,Type,Info), Tree, Leaf).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_lookup(+Key,?Type,?Field,+Dictionary,?Info)
   :: atm * atm * term * dictionary * atm
   # "The predicate will search for the Key and return Info;defined or
       undefined accordingly. If defined the fields will be filled as well.
       The predicate do not insert the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_lookup(Name,Type,Field,Dictionary,Info) :-
	%name(Name, Key),
	dictionary_lookup(element(Name,Type,Field),Dictionary,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_lookup(+Element,+Tree,-Info)
   :: element * tree * term
   # "The predicate will search for the element and return Info;defined or
       undefined accordingly. If defined the fields will be filled as well.
       The predicate do not insert the element.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_lookup(E,Tree,Info) :-
	scan_tree(E,Tree,Info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- pred dictionary_insert(+Key,+Type,+Field,+Dictionary,?Info)
   :: atm * atm * term * dictionary * atm
   # "The predicate will search for the place for the Key and return Info
       if the element inserted had a post before (same key value), new and 
       multiple. The dictionary is dynamic and do not need output because 
       of using unbinded variables.".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dictionary_insert(Name,Type,Field,Dictionary,Info) :-
	%name(Name, Key),
	dictionary_seek(element(Name,Type,Field),Dictionary,Leaf),
	(Leaf == new
	->
	Info = new
	; ( element_not_on_branch(element(Name,Type,Field),Leaf)
	  ->get_leaf_rest(Leaf, Rest_of_leafs),
	    get_last_leaf(Rest_of_leafs,Last_leaf),
	    create_leaf(element(Name,Type,Field),Last_leaf),
	    Info = multiple
	  ; true
	  ) 
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   We do not want duplicates
dictionary_insert(element(Name,Type,Field),Dictionary,Info) :-
	%name(Name, Key),
	dictionary_seek(element(Name,Type,Field),Dictionary,Leaf),
	(Leaf == new
	->
	Info = new
	; ( element_not_on_branch(element(Name,Type,Field),Leaf)
	  -> get_leaf_rest(Leaf, Rest_of_leafs),
	     get_last_leaf(Rest_of_leafs,Last_leaf),
	     create_leaf(element(Name,Type,Field),Last_leaf),
	     Info = multiple
	  ;  true
	  )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Looking up the value, returns the whole leaf with its possible parts.
climb_tree(Element,Tree,Leaf) :-
	get_element_key(Element,Element_key),
	get_tree_key(Tree,Tree_key),
	compare_key_eq(Element_key,Tree_key),
	!,
	get_tree_leaf(Tree,Leaf).

%Traversing
climb_tree(Element,Tree,Leaf) :-
	get_element_key(Element,Key),
	change_direction(Key,Tree,Branch),
	climb_tree(Element,Branch,Leaf).


%Inserting a new leaf.
climb_tree(Element,Tree,new) :-
	create_tree(Element,Tree),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%We just want to lookup or there is an error.
scan_tree(Element,Tree,Info) :-
	get_element_key(Element,Element_key),
	get_tree_key(Tree,Tree_key),
	compare_key_eq(Element_key,Tree_key),
	!,
	get_tree_leaf(Tree,Leaf),
	get_leaf_type(Leaf,Leaf_type),
	get_element_type(Element,Element_type),
	(Leaf_type == Element_type
	->
	get_element_dic(Element,Dic),
	get_leaf_information(Leaf,Dic),
	Info = defined
	;
	scan_correct_leaf(Element,Leaf,Info)).

%Traversing to find the post
scan_tree(Element,Tree,Info) :-
	get_element_key(Element,Key),
	change_direction(Key,Tree,Branch),
	scan_tree(Element,Branch,Info).

%The post could not be found
scan_correct_leaf(Element,Leaf,Info) :-
	get_next_leaf(Leaf,Next),
	(Next == last
	->
	Info = undefined
	;
	get_leaf_type(Leaf,Leaf_type),
	get_element_type(Element,Element_type),
	(Leaf_type == Element_type
	->
	get_element_dic(Element,Dic),
	get_leaf_information(Leaf,Dic),
	Info = defined
	;
	scan_correct_leaf(Element,Leaf,Info))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merge_tree(Tree0,Tree1) :-
	write(Tree0),nl,
	get_all_leafs(Tree0,Leafs),
	write(Leafs),nl,
	make_elements_of_leafs(Leafs,Elements),
	write(Elements),nl,
	insert_elements(Elements,Tree1).

gather_leafs(Leaf,Leafs) :-
	is_leaf(Leaf),
	get_next_leaf(Leaf,Rest),
	(  Rest == last
	-> Leafs = [Leaf]
	;  gather_leafs(Rest,More),
	   Leafs = [Leaf|More]
	).


get_all_leafs(Tree,Leafs) :-
	is_tree(Tree),
	get_tree_leaf(Tree,TreeLeaf),
	gather_leafs(TreeLeaf,TreeLeafs),
	(  got_left_tree(Tree)
	-> get_left_tree(Tree,Left),
	   get_all_leafs(Left,LeftLeafs)
	;  LeftLeafs = []
	),
	(  got_right_tree(Tree)
	-> get_right_tree(Tree,Right),
	   get_all_leafs(Right,RightLeafs)
	;  RightLeafs = []
	),
	append(TreeLeafs,LeftLeafs,L0),
	append(L0,RightLeafs,Leafs).

make_elements_of_leafs([],[]).
make_elements_of_leafs([Leaf|Rest_of_leafs],[Element|Rest_of_elements]) :-
	create_leaf(Element,Leaf),
	make_elements_of_leafs(Rest_of_leafs,Rest_of_elements).


insert_elements([],_Tree).
insert_elements([E|Rest],Tree) :-
	dictionary_insert(E,Tree,_Info),
	insert_elements(Rest,Tree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compare_number_eq(K0,K1) :-
	number(K0),
	number(K1),
	k0 == K1.

compare_number_leq(K0,K1) :-
	number(K0),
	number(K1),
	K0 =< K1.

compare_number_lt(K0,K1) :-
	number(K0),
	number(K1),
	K0 < K1.

compare_number_gt(K0,K1) :-
	number(K0),
	number(K1),
	K0 > K1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compare_list_eq(K0,K1) :-
	K0 == K1.

compare_list_leq(K0,K1) :-
	nonvar(K0),
	nonvar(K1),
	name(K0,K0_list),
	name(K1,K1_list),
	!,
	list_leq(K0_list,K1_list).

compare_list_lt(K0,K1) :-
	nonvar(K0),
	nonvar(K1),
	name(K0,K0_list),
	name(K1,K1_list),
	!,
	list_lt(K0_list,K1_list).

compare_list_gt(K0,K1) :-
	nonvar(K0),
	nonvar(K1),
	name(K0,K0_list),
	name(K1,K1_list),
	!,
	list_gt(K0_list,K1_list).

%%%%%%%%%%%%%%%%
list_eq([],[]).
list_eq([S|R0],[S|R1]) :-
	list_eq(R0,R1).

%%%%%%%%%%%%%%%%
list_leq(L0,L1) :-
	list_gt(L1,L0).

%%%%%%%%%%%%%%%%
list_lt([],[_]).
list_lt([S0|_],[S1|_]) :-
	S0 < S1.

list_lt([S|R0],[S|R1]) :-
	list_lt(R0,R1).

%%%%%%%%%%%%%%%%
list_gt([_],[]).
list_gt([S0|_],[S1|_]) :-
	S0 >= S1.

list_gt([S|R0],[S|R1]) :-
	list_gt(R0,R1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_tree(T) :-
	is_tree(T),
	get_tree_leaf(T,F),
	print_leaf(F),
	get_left_tree(T,L),
	print_tree(L),
	get_right_tree(T,R),
	print_tree(R).

print_tree(_).

print_leaf(leaf(Val,Type,Info)) :-
	write(Val),
	write(' '),
	write(Type),
	write(' '),
	write(Info),
	nl.

print_leaf(_).





test(A0,B0) :-
	create_tree(A0),
	create_tree(B0),
	dictionary_insert(a,typa,a,A0,_),
	dictionary_insert(b,typa,b,A0,_),
	dictionary_insert(b,typa,bb,A0,_),
	dictionary_insert(a,typa,a,A0,_),
	dictionary_insert(c,typa,a,A0,_),
	dictionary_insert(c,typc,a,A0,_),
	dictionary_insert(d,typd,a,A0,_),

	dictionary_insert(a,typa,a,B0,_),
	dictionary_insert(b,typa,b,B0,_),
	dictionary_insert(b,typa,bb,B0,_),
	dictionary_insert(a,typa,a,B0,_),
	dictionary_insert(c,typa,a,B0,_),
	dictionary_insert(e,type,e,B0,_),

	merge_tree(A0,B0).
