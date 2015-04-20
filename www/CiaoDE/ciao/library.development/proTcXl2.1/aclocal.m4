dnl
dnl SccsId[]="@(#)aclocal.m4	1.3 9/13/94"
dnl

define(PT_HAVE_LIB,
[
    echo checking for $1 library
    libs="/opt/unsupported/lib /usr/local/unsupported/lib /usr/local/lib /opt/packages/lib"
    if test ! -z "$inst_dir"; then
	libs="$inst_dir $inst_dir/lib $inst_dir/tcl*/lib $inst_dir/tk*/lib $libs"
    fi
    for l in $libs; do
	for f in $l/lib$1$2.a $l/lib$1$2.so $l/lib$1$2.so.*; do
	    if test -f "$f"; then
		$1[_library]=$l
		test -n "$verbose" && echo "	setting $1 library to $l"
		break 2
	    fi
	done
    done
    if test -z "[$]$1[_library]"; then
    	echo "...cannot find the $1 library"
	exit
    fi

    AC_SUBST($1[_library])
])dnl

define(PT_HAVE_TESTS,
[
    echo checking for $1 test files
    libs="/usr/local/unsupported/src /opt/unsupported/src"
    if test ! -z "$src_dir"; then
	libs="$src_dir $libs"
    fi
    for l in $libs; do
	for f in $l/$1*/tests $l/tcl/$1*/tests; do
	    if test -d "$f"; then
		$1[_tests]=$f
		test -n "$verbose" && echo "	setting $1 tests to $f"
		break 2
	    fi
	done
    done
    if test -z "[$]$1[_tests]"; then
    	echo "...cannot find the $1 tests, proceeding without them"
	$1[_tests]="''"
    fi

    AC_SUBST($1[_tests])
])dnl

define(PT_HAVE_INCLUDE,
[
    echo checking for $1 include file
    incs="/opt/unsupported/include /usr/local/unsupported/include /usr/local/include /opt/packages/include"
    if test ! -z "$inst_dir"; then
	incs="$inst_dir/include $inst_dir/$1*/include $incs"
    fi
    for i in $incs; do
	if test -f "$i/$1.h"; then
	    $1[_include]=$i
	    test -n "$verbose" && echo "	setting $1 include to $i"
	    break
	fi
    done
    if test -z "[$]$1[_include]"; then
    	echo "...cannot find the $1 include file"
	exit
    fi

    AC_SUBST($1[_include])
])dnl

