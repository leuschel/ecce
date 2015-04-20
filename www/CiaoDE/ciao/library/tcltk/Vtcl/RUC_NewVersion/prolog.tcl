proc prolog_command {term} {

 global event_socket
 global term_socket
 global prolog_variables

 set result 0

 puts $event_socket execute($term).
 flush $event_socket

 gets $term_socket result


 set ret [unify_term_aux $term $result]
}

proc prolog_variables_out {} {

 global prolog_variables
 
 set var_name 0
 set l [array get prolog_variables]
 
 set max [llength $l]

 for {set pos 0} {$pos<$max} {} {
     set var_name [lindex $l $pos]
     incr pos
     global $var_name
     set $var_name [lindex $l $pos]
     incr pos
 }

}



proc prolog_exit {} {
    prolog_command exit_tk_event_loop
}


