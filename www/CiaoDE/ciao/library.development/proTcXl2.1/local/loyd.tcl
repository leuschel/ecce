# The Loyd's puzzle

# Procedure invoked by buttons in the puzzle to resize the puzzle entries:

proc Lpuzzle.switch {w num} {
    global xint yint xintsize yintsize
    global lastn move lastdir cmoves lastmove
    if {$num != $lastn} {
	set move down
	set lastdir 0
    }
    set nl1 0
    for {set i 4} {$i > 0} {incr i -1} {
	############
	# move down:
	case $move {
	"down" {
	    # size 1:
	    if { ($xintsize($num) == 1) &&
	    ($xint(space1) == $xint($num)) &&
	    ($yint(space1) == $yint($num) + $yintsize($num))} {
		set yint(space1) $yint($num)
		set yint($num) [expr $yint($num) + 1]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 down
		break
	    } else {if {($xintsize($num) == 1) &&
	    ($xint(space2) == $xint($num)) &&
	    ($yint(space2) == $yint($num) + $yintsize($num))} {
		set yint(space2) $yint($num)
		set yint($num) [expr $yint($num) + 1]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 down
		break
	    } else {if {($xintsize($num) == 2) &&
	    ($yint(space1) == $yint($num) + $yintsize($num)) &&
	    ($yint(space2) == $yint($num) + $yintsize($num)) &&
	    (($xint(space1) == $xint($num)) &&
		($xint(space2) == $xint($num) + 1) ||
	    ($xint(space2) == $xint($num)) &&
		($xint(space1) == $xint($num) + 1))} {
	# size 2:
		set yint(space1) $yint($num)
		set yint(space2) $yint($num)
		set yint($num) [expr $yint($num) + 1]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 down
		break
	    } else {
		if {$lastdir == 1} {
		    set move up
		} else {
		    set move left
		}
	    }}}
	}
	############
	# move left:
	"left" {
	    # size 1:
	    if {($yintsize($num) == 1) &&
	    ($yint(space1) == $yint($num)) &&
	    ($xint(space1) == $xint($num) - 1)} {
		set xint($num) $xint(space1)
		set xint(space1) [expr $xint($num) + $xintsize($num)]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 left
		break
	    } else {if {($yintsize($num) == 1) &&
	    ($yint(space2) == $yint($num)) &&
	    ($xint(space2) == $xint($num) - 1)} {
		set xint($num) $xint(space2)
		set xint(space2) [expr $xint($num) + $xintsize($num)]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 left
		break
	    } else {if {($yintsize($num) == 2) &&
	    ($xint(space1) == $xint($num) - 1) &&
	    ($xint(space2) == $xint($num) - 1) &&
	    (($yint(space1) == $yint($num)) &&
		($yint(space2) == $yint($num) + 1) ||
	    ($yint(space2) == $yint($num)) &&
		($yint(space1) == $yint($num) + 1))} {
	# size 2:
		set xint(space1) [expr $xint($num) + $xintsize($num) - 1]
		set xint($num) $xint(space2)
		set xint(space2) $xint(space1)
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 left
		break
	    } else {
		if {$lastdir == 1} {
		    set move down
		} else {
		    set move right
		}
	    }}}
	}
	############
	# move right:
	"right" {
	# size 1:
	    if {($yintsize($num) == 1) &&
	    ($yint(space1) == $yint($num)) &&
	    ($xint(space1) == $xint($num) + $xintsize($num))} {
		set xint(space1) $xint($num)
		set xint($num) [expr $xint($num) + 1]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 right
		break
	    } else {if {($yintsize($num) == 1) &&
	    ($yint(space2) == $yint($num)) &&
	    ($xint(space2) == $xint($num) + $xintsize($num))} {
		set xint(space2) $xint($num)
		set xint($num) [expr $xint($num) + 1]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 right
		break
	    } else {if {($yintsize($num) == 2) &&
	    ($xint(space1) == $xint($num) + $xintsize($num)) &&
	    ($xint(space2) == $xint($num) + $xintsize($num)) &&
	    (($yint(space1) == $yint($num)) &&
		($yint(space2) == $yint($num) + 1) ||
	    ($yint(space2) == $yint($num)) &&
		($yint(space1) == $yint($num) + 1))} {
	# size 2:
		set xint(space1) $xint($num)
		set xint(space2) $xint($num)
		set xint($num) [expr $xint($num) + 1]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 right
		break
	    } else {
		if {$lastdir == 1} {
		    set move left
		} else {
		    set move up
		}
	    }}}
	}
	############
	# move up:
	"up" {
	    # size 1:
	    if {($xintsize($num) == 1) &&
	    ($xint(space1) == $xint($num)) &&
	    ($yint(space1) == $yint($num) - 1)} {
		set yint($num) $yint(space1)
		set yint(space1) [expr $yint($num) + $yintsize($num)]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 up
		break
	    } else {if {($xintsize($num) == 1) &&
	    ($xint(space2) == $xint($num)) &&
	    ($yint(space2) == $yint($num) - 1)} {
		set yint($num) $yint(space2)
		set yint(space2) [expr $yint($num) + $yintsize($num)]
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 up
		break
	    } else {if {($xintsize($num) == 2) &&
	    ($yint(space1) == $yint($num) - 1) &&
	    ($yint(space2) == $yint($num) - 1) &&
	    (($xint(space1) == $xint($num)) &&
		($xint(space2) == $xint($num) + 1) ||
	    ($xint(space2) == $xint($num)) &&
		($xint(space1) == $xint($num) + 1))} {
	# size 2:
		set yint(space1) [expr $yint($num) + $yintsize($num) - 1]
		set yint($num) $yint(space2)
		set yint(space2) $yint(space1)
		set x [expr $xint($num)*.2]
		set y [expr $yint($num)*.25]
		set nl1 up
		break
	    } else {
		if {$lastdir == 1} {
		    set move right
		} else {
		    set move down
		}
	    }}}
	}}
    }
    if {$nl1 != 0} {
	if {($nl1 != $lastmove) || ($lastn != $num)} {
	    incr cmoves 1
	}
	set lastmove $nl1
	if {$lastn != $num} {
	    set lastn $num
	}
	if {($move == "up") || ($move == "left")} {
	    set lastdir 1
	} else {
	    set lastdir 0
	}
	place $w.frame.$num -relx $x -rely $y
    }
}

proc reset {w} {
    global xpos ypos xsize ysize
    global xint yint xintsize yintsize
    global lastn cmoves lastmove
    set sizes { {2 1} {2 1} {1 1} {1 1} {2 2} {1 2} {1 2} {1 2} {1 2} }
    set pos { {0 0} {0 1} {2 0} {2 1} {3 0} {0 2} {1 2} {3 2} {4 2} }
    for {set i 0} {$i < 9} {set i [expr $i+1]} {
	set num [expr $i+1]
	set ppos [lindex $pos $i]
	set psize [lindex $sizes $i]
	set xpos($num) [expr ([lindex $ppos 0])*.2]
	set ypos($num) [expr ([lindex $ppos 1])*.25]
	set xsize($num) [expr [lindex $psize 0]*.2]
	set ysize($num) [expr [lindex $psize 1]*.25]
	set xint($num) [lindex $ppos 0]
	set yint($num) [lindex $ppos 1]
	set xintsize($num) [lindex $psize 0]
	set yintsize($num) [lindex $psize 1]
	place $w.frame.$num -relx $xpos($num) -rely $ypos($num) \
	    -relwidth $xsize($num) -relheight $ysize($num)
    }
    set xint(space1) 2
    set yint(space1) 2
    set xint(space2) 2
    set yint(space2) 3
    set lastn 0
    set cmoves 0
    set lastmove 0
}

proc loyd {{w .p1}} {
    global xpos ypos xsize ysize
    global xint yint xintsize yintsize
    global lastn cmoves
    set w .p1
    set cmoves 0
    catch {destroy $w}
    toplevel $w
    dpos $w
    wm title $w "Loyd Puzzle"
    wm iconname $w "Loyd Puzzle"

    message $w.msg -font -Adobe-times-medium-r-normal--*-140* -aspect 300 \
		-text "The goal of the Loyd puzzle is to move the large square from the upper right corner to the upper left corner."
    frame $w.frame -geometry 150x120 -borderwidth 2 -relief sunken \
	-bg Bisque3
    frame $w.mf
    label $w.mf.movl -text Moves:
    entry $w.mf.moves -width 6 -relief sunken -textvariable cmoves
    pack $w.msg -side top
    pack $w.mf.movl $w.mf.moves -side left -padx 5 -pady 5
    pack $w.mf -side top
    pack $w.frame -side top -padx 5 -pady 5
    button $w.ok -text Exit -command "destroy $w"
    button $w.res -text Reset -command "reset $w"
    pack $w.res $w.ok -side left -expand yes -fill x
    for {set i 1} {$i < 10} {set i [expr $i+1]} {
	button $w.frame.$i -relief raised -text $i \
		-command "Lpuzzle.switch $w $i"
    }
    reset $w
}
