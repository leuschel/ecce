#!/bin/sh ciaowish_temp
#############################################################################
# Visual Tcl v1.11p1 Project
#

#################################
# GLOBAL VARIABLES
#
global widget; 
    set widget(phone_window) {.top18}
    set widget(rev,.top18) {phone_window}
    set widget(rev,.top30) {tutorial_window}
    set widget(tutorial_window) {.top30}

#################################
# USER DEFINED PROCEDURES
#
proc init {argc argv} {
global PhoneName
global PhoneNumber
set PhoneName "John"
set PhoneNumber "000-0000"
wm withdraw .
use_module vtcl_example
}

init $argc $argv


proc {main} {argc argv} {

}

proc {Window} {args} {
global vTcl
    set cmd [lindex $args 0]
    set name [lindex $args 1]
    set newname [lindex $args 2]
    set rest [lrange $args 3 end]
    if {$name == "" || $cmd == ""} {return}
    if {$newname == ""} {
        set newname $name
    }
    set exists [winfo exists $newname]
    switch $cmd {
        show {
            if {$exists == "1" && $name != "."} {wm deiconify $name; return}
            if {[info procs vTclWindow(pre)$name] != ""} {
                eval "vTclWindow(pre)$name $newname $rest"
            }
            if {[info procs vTclWindow$name] != ""} {
                eval "vTclWindow$name $newname $rest"
            }
            if {[info procs vTclWindow(post)$name] != ""} {
                eval "vTclWindow(post)$name $newname $rest"
            }
        }
        hide    { if $exists {wm withdraw $newname; return} }
        iconify { if $exists {wm iconify $newname; return} }
        destroy { if $exists {destroy $newname; return} }
    }
}

#################################
# VTCL GENERATED GUI PROCEDURES
#

proc vTclWindow. {base} {
    if {$base == ""} {
        set base .
    }
    ###################
    # CREATING WIDGETS
    ###################
    wm focusmodel $base passive
    wm geometry $base 200x200+6+26
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm withdraw $base
    wm title $base "wish"
    ###################
    # SETTING GEOMETRY
    ###################
}

proc vTclWindow.top17 {base} {
    if {$base == ""} {
        set base .top17
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -menu .top17.m26 
    wm focusmodel $base passive
    wm geometry $base 549x236+366+311
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 0 0
    wm deiconify $base
    wm title $base "CIAOWISH Example - A simple phone database
"
    bind $base <Destroy> {
        exit
    }
    menubutton $base.men20 \
        -font {lucidatypewriter -12 bold} -foreground #2800d4 -indicatoron 1 \
        -menu .top17.men20.m -padx 4 -pady 3 -relief raised \
        -text {Database options} 
    menu $base.men20.m \
        -cursor {} -tearoff 0 
    $base.men20.m add command \
        \
        -command {if {[winfo exists $widget(phone_window)]} {
   wm deiconify $widget(phone_window)
} else {
   vTclWindow$widget(phone_window) ""
}} \
        -label {Phone look-up} 
    $base.men20.m add separator
    $base.men20.m add command \
        -command exit -label Exit 
    message $base.mes21 \
        -font {helvetica -12 bold} -foreground #fe0000 -padx 5 -pady 2 \
        -text {This is an example on CIAOWISH and CIAOVTCL facilities.} \
        -width 400 
    message $base.mes22 \
        -foreground #0000fe -padx 5 -pady 2 \
        -text {CIAOWISH is a substitute for WISH tcl interpreter which enables a Tcl/Tk programmer to call CIAO/Prolog code.} \
        -width 504 
    message $base.mes23 \
        -foreground #008efe -padx 5 -pady 2 \
        -text {This example application was developed using GNU Visual TCL, running under CIAOWISH. This is wich we call CIAOVTCL.} \
        -width 459 
    message $base.mes24 \
        -foreground #ea9472 -padx 5 -pady 2 \
        -text {CIAOVTCL is able to invoke Prolog code at design time !!!.} \
        -width 427 
    message $base.mes25 \
        -foreground #006200 -padx 5 -pady 2 \
        -text {CIAOVTCL generated scripts runs with no further modifications.} \
        -width 414 
    message $base.mes27 \
        -padx 5 -pady 2 -relief ridge \
        -text {This example uses a Prolog database. This Prolog code can be found in tcl_example.pl} \
        -width 342 
    button $base.but28 \
        \
        -command {if {[winfo exists $widget(tutorial_window)]} {
   wm deiconify $widget(tutorial_window)
} else {
   vTclWindow$widget(tutorial_window) ""
}} \
        -font {lucidabright -12 bold} -foreground #526c44 -padx 9 -pady 3 \
        -text {Press here for a short tutorial in using CIAOVTCL / CIAOWISH} \
        -width 88 
    menu $base.m26 \
        -cursor {} 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.men20 \
        -in .top17 -anchor center -expand 0 -fill x -side top 
    pack $base.mes21 \
        -in .top17 -anchor nw -expand 0 -fill x -side top 
    pack $base.mes22 \
        -in .top17 -anchor center -expand 0 -fill x -side top 
    pack $base.mes23 \
        -in .top17 -anchor center -expand 0 -fill x -side top 
    pack $base.mes24 \
        -in .top17 -anchor center -expand 0 -fill x -side top 
    pack $base.mes25 \
        -in .top17 -anchor center -expand 0 -fill x -side top 
    pack $base.mes27 \
        -in .top17 -anchor center -expand 0 -fill none -side top 
    pack $base.but28 \
        -in .top17 -anchor center -expand 1 -fill both -side top 
}

proc vTclWindow.top30 {base} {
    if {$base == ""} {
        set base .top30
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -background #d9d9c6 -menu .top30.m31 -relief raised 
    wm focusmodel $base passive
    wm geometry $base 643x278+33+490
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 1 1
    wm title $base "CIAOVTCL Tutorial"
    message $base.mes33 \
        -background #d9d9c6 -foreground #f80000 -padx 5 -pady 2 \
        -text {Follow this steps in order to design a Tcl/Tk application which calls CIAO/Prolog:} \
        -width 515 
    message $base.mes34 \
        -background #d9d9c8 -foreground #0000fe -padx 5 -pady 2 \
        -text {- Run  CIAOVTCL} -width 183 
    message $base.mes35 \
        -background #d9d9ca -foreground #005000 -padx 5 -pady 2 \
        -text {- Create a new function using "ADD" at the "Function List" dialog. This function will load CIAO/Prolog modules.} \
        -width 496 
    message $base.mes36 \
        -background #d9d9c4 -foreground #00a400 -padx 5 -pady 2 \
        -text {- At this function, load any Prolog module with use_module. For example: use_module vtcl_example} \
        -width 440 
    message $base.mes37 \
        -background #d9d9c8 -foreground #8a0062 -padx 5 -pady 2 \
        -text {- Open the "init" function and write a call to the previous one. If you want to test prolog code at design time, you must also open the "Command console" and call your function manually.} \
        -width 495 
    message $base.mes38 \
        -background #d9d9c8 -foreground #009a7e -padx 5 -pady 2 \
        -text {- Attach prolog calls with "prolog" function. For example: prolog vtcl_example:data_base('Jhon',Phone). Results will be stored in global variable "prolog_variables(variable_name)". For this example: prolog_variables(Phone) will hold the retrieved data.} \
        -width 638 
    message $base.mes39 \
        -background #d9d9c6 -foreground #005000 -padx 5 -pady 2 \
        -text {- Notice that module qualification is needed.} -width 360 
    message $base.mes40 \
        -background #d9d9c4 -padx 5 -pady 2 \
        -text {- Save your project, and change file permission in order to make it executable.} \
        -width 567 
    menu $base.m31 \
        -cursor {} 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.mes33 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes34 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes35 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes36 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes37 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes38 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes39 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
    pack $base.mes40 \
        -in .top30 -anchor center -expand 0 -fill x -side top 
}

proc vTclWindow.top18 {base} {
    if {$base == ""} {
        set base .top18
    }
    if {[winfo exists $base]} {
        wm deiconify $base; return
    }
    ###################
    # CREATING WIDGETS
    ###################
    toplevel $base -class Toplevel \
        -cursor xterm -menu .top18.m20 
    wm focusmodel $base passive
    wm geometry $base 333x187+457+75
    wm maxsize $base 1137 870
    wm minsize $base 1 1
    wm overrideredirect $base 0
    wm resizable $base 0 0
    wm deiconify $base
    wm title $base "Phone look up"
    message $base.mes19 \
        -anchor nw -font {helvetica 14 bold} -foreground #0088d6 -padx 5 \
        -pady 2 -text {Enter name to search for:} -width 261 
    entry $base.ent21 \
        -background #d9d922 -borderwidth 4 \
        -font {lucidabright 11 {bold italic}} -textvariable PhoneName \
        -width 29 
    button $base.but22 \
        -borderwidth 4 -font {lucida 18 bold} -foreground #be82fc -padx 11 \
        -pady 4 -text Submit 
    message $base.mes23 \
        -font {helvetica 14 bold} -foreground #00a8ea -padx 5 -pady 2 \
        -text {Phone number:} -width 155 
    message $base.mes24 \
        -background #d9d938 -font {Helvetica 18 bold} -foreground #f40000 \
        -padx 5 -pady 2 -relief sunken -text 000 -textvariable PhoneNumber \
        -width 175 
    menu $base.m20 \
        -cursor {} 
    ###################
    # SETTING GEOMETRY
    ###################
    pack $base.mes19 \
        -in .top18 -anchor center -expand 0 -fill x -side top 
    pack $base.ent21 \
        -in .top18 -anchor n -expand 0 -fill none -side top 
    pack $base.but22 \
        -in .top18 -anchor center -expand 0 -fill none -ipady 3 -padx 5 \
        -pady 5 -side top 
    pack $base.mes23 \
        -in .top18 -anchor nw -expand 0 -fill none -side top 
    pack $base.mes24 \
        -in .top18 -anchor center -expand 0 -fill x -side top 
}

Window show .
Window show .top17
Window show .top18

main $argc $argv
