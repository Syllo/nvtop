dnl***************************************************************************
dnl Copyright 2018-2020,2021 Thomas E. Dickey                                *
dnl Copyright 2003-2017,2018 Free Software Foundation, Inc.                  *
dnl                                                                          *
dnl Permission is hereby granted, free of charge, to any person obtaining a  *
dnl copy of this software and associated documentation files (the            *
dnl "Software"), to deal in the Software without restriction, including      *
dnl without limitation the rights to use, copy, modify, merge, publish,      *
dnl distribute, distribute with modifications, sublicense, and/or sell       *
dnl copies of the Software, and to permit persons to whom the Software is    *
dnl furnished to do so, subject to the following conditions:                 *
dnl                                                                          *
dnl The above copyright notice and this permission notice shall be included  *
dnl in all copies or substantial portions of the Software.                   *
dnl                                                                          *
dnl THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
dnl OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
dnl MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
dnl IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
dnl DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
dnl OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
dnl THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
dnl                                                                          *
dnl Except as contained in this notice, the name(s) of the above copyright   *
dnl holders shall not be used in advertising or otherwise to promote the     *
dnl sale, use or other dealings in this Software without prior written       *
dnl authorization.                                                           *
dnl***************************************************************************
dnl
dnl $Id: aclocal.m4,v 1.202 2021/10/11 00:18:09 tom Exp $
dnl
dnl Author: Thomas E. Dickey
dnl
dnl Macros used in NCURSES test programs auto-configuration script.
dnl
dnl These macros are maintained separately from NCURSES.  The copyright on
dnl this file applies to the aggregation of macros and does not affect use of
dnl these macros in other applications.
dnl
dnl See these pages for additional information:
dnl		https://invisible-island.net/autoconf/
dnl		https://invisible-island.net/autoconf/my-autoconf.html
dnl
dnl ---------------------------------------------------------------------------
dnl ---------------------------------------------------------------------------
dnl AM_LANGINFO_CODESET version: 6 updated: 2021/01/01 16:53:59
dnl -------------------
dnl Inserted as requested by gettext 0.10.40
dnl File from /usr/share/aclocal
dnl codeset.m4
dnl ====================
dnl serial AM1
dnl
dnl From Bruno Haible.
AC_DEFUN([AM_LANGINFO_CODESET],
[
AC_CACHE_CHECK([for nl_langinfo and CODESET], am_cv_langinfo_codeset,
	[AC_TRY_LINK([#include <langinfo.h>],
	[char* cs = nl_langinfo(CODESET); (void)cs],
	am_cv_langinfo_codeset=yes,
	am_cv_langinfo_codeset=no)
	])
	if test "$am_cv_langinfo_codeset" = yes; then
		AC_DEFINE(HAVE_LANGINFO_CODESET, 1,
		[Define if you have <langinfo.h> and nl_langinfo(CODESET).])
	fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ACVERSION_CHECK version: 5 updated: 2014/06/04 19:11:49
dnl ------------------
dnl Conditionally generate script according to whether we're using a given autoconf.
dnl
dnl $1 = version to compare against
dnl $2 = code to use if AC_ACVERSION is at least as high as $1.
dnl $3 = code to use if AC_ACVERSION is older than $1.
define([CF_ACVERSION_CHECK],
[
ifdef([AC_ACVERSION], ,[ifdef([AC_AUTOCONF_VERSION],[m4_copy([AC_AUTOCONF_VERSION],[AC_ACVERSION])],[m4_copy([m4_PACKAGE_VERSION],[AC_ACVERSION])])])dnl
ifdef([m4_version_compare],
[m4_if(m4_version_compare(m4_defn([AC_ACVERSION]), [$1]), -1, [$3], [$2])],
[CF_ACVERSION_COMPARE(
AC_PREREQ_CANON(AC_PREREQ_SPLIT([$1])),
AC_PREREQ_CANON(AC_PREREQ_SPLIT(AC_ACVERSION)), AC_ACVERSION, [$2], [$3])])])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ACVERSION_COMPARE version: 3 updated: 2012/10/03 18:39:53
dnl --------------------
dnl CF_ACVERSION_COMPARE(MAJOR1, MINOR1, TERNARY1,
dnl                      MAJOR2, MINOR2, TERNARY2,
dnl                      PRINTABLE2, not FOUND, FOUND)
define([CF_ACVERSION_COMPARE],
[ifelse(builtin([eval], [$2 < $5]), 1,
[ifelse([$8], , ,[$8])],
[ifelse([$9], , ,[$9])])])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_CFLAGS version: 15 updated: 2020/12/31 10:54:15
dnl -------------
dnl Copy non-preprocessor flags to $CFLAGS, preprocessor flags to $CPPFLAGS
dnl $1 = flags to add
dnl $2 = if given makes this macro verbose.
dnl
dnl Put any preprocessor definitions that use quoted strings in $EXTRA_CPPFLAGS,
dnl to simplify use of $CPPFLAGS in compiler checks, etc., that are easily
dnl confused by the quotes (which require backslashes to keep them usable).
AC_DEFUN([CF_ADD_CFLAGS],
[
cf_fix_cppflags=no
cf_new_cflags=
cf_new_cppflags=
cf_new_extra_cppflags=

for cf_add_cflags in $1
do
case "$cf_fix_cppflags" in
(no)
	case "$cf_add_cflags" in
	(-undef|-nostdinc*|-I*|-D*|-U*|-E|-P|-C)
		case "$cf_add_cflags" in
		(-D*)
			cf_tst_cflags=`echo "${cf_add_cflags}" |sed -e 's/^-D[[^=]]*='\''\"[[^"]]*//'`

			test "x${cf_add_cflags}" != "x${cf_tst_cflags}" \
				&& test -z "${cf_tst_cflags}" \
				&& cf_fix_cppflags=yes

			if test "$cf_fix_cppflags" = yes ; then
				CF_APPEND_TEXT(cf_new_extra_cppflags,$cf_add_cflags)
				continue
			elif test "${cf_tst_cflags}" = "\"'" ; then
				CF_APPEND_TEXT(cf_new_extra_cppflags,$cf_add_cflags)
				continue
			fi
			;;
		esac
		case "$CPPFLAGS" in
		(*$cf_add_cflags)
			;;
		(*)
			case "$cf_add_cflags" in
			(-D*)
				cf_tst_cppflags=`echo "x$cf_add_cflags" | sed -e 's/^...//' -e 's/=.*//'`
				CF_REMOVE_DEFINE(CPPFLAGS,$CPPFLAGS,$cf_tst_cppflags)
				;;
			esac
			CF_APPEND_TEXT(cf_new_cppflags,$cf_add_cflags)
			;;
		esac
		;;
	(*)
		CF_APPEND_TEXT(cf_new_cflags,$cf_add_cflags)
		;;
	esac
	;;
(yes)
	CF_APPEND_TEXT(cf_new_extra_cppflags,$cf_add_cflags)

	cf_tst_cflags=`echo "${cf_add_cflags}" |sed -e 's/^[[^"]]*"'\''//'`

	test "x${cf_add_cflags}" != "x${cf_tst_cflags}" \
		&& test -z "${cf_tst_cflags}" \
		&& cf_fix_cppflags=no
	;;
esac
done

if test -n "$cf_new_cflags" ; then
	ifelse([$2],,,[CF_VERBOSE(add to \$CFLAGS $cf_new_cflags)])
	CF_APPEND_TEXT(CFLAGS,$cf_new_cflags)
fi

if test -n "$cf_new_cppflags" ; then
	ifelse([$2],,,[CF_VERBOSE(add to \$CPPFLAGS $cf_new_cppflags)])
	CF_APPEND_TEXT(CPPFLAGS,$cf_new_cppflags)
fi

if test -n "$cf_new_extra_cppflags" ; then
	ifelse([$2],,,[CF_VERBOSE(add to \$EXTRA_CPPFLAGS $cf_new_extra_cppflags)])
	CF_APPEND_TEXT(EXTRA_CPPFLAGS,$cf_new_extra_cppflags)
fi

AC_SUBST(EXTRA_CPPFLAGS)

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_INCDIR version: 17 updated: 2021/09/04 06:35:04
dnl -------------
dnl Add an include-directory to $CPPFLAGS.  Don't add /usr/include, since it is
dnl redundant.  We don't normally need to add -I/usr/local/include for gcc,
dnl but old versions (and some misinstalled ones) need that.  To make things
dnl worse, gcc 3.x may give error messages if -I/usr/local/include is added to
dnl the include-path).
AC_DEFUN([CF_ADD_INCDIR],
[
if test -n "$1" ; then
  for cf_add_incdir in $1
  do
	while test "$cf_add_incdir" != /usr/include
	do
	  if test -d "$cf_add_incdir"
	  then
		cf_have_incdir=no
		if test -n "$CFLAGS$CPPFLAGS" ; then
		  # a loop is needed to ensure we can add subdirs of existing dirs
		  for cf_test_incdir in $CFLAGS $CPPFLAGS ; do
			if test ".$cf_test_incdir" = ".-I$cf_add_incdir" ; then
			  cf_have_incdir=yes; break
			fi
		  done
		fi

		if test "$cf_have_incdir" = no ; then
		  if test "$cf_add_incdir" = /usr/local/include ; then
			if test "$GCC" = yes
			then
			  cf_save_CPPFLAGS=$CPPFLAGS
			  CF_APPEND_TEXT(CPPFLAGS,-I$cf_add_incdir)
			  AC_TRY_COMPILE([#include <stdio.h>],
				  [printf("Hello")],
				  [],
				  [cf_have_incdir=yes])
			  CPPFLAGS=$cf_save_CPPFLAGS
			fi
		  fi
		fi

		if test "$cf_have_incdir" = no ; then
		  CF_VERBOSE(adding $cf_add_incdir to include-path)
		  ifelse([$2],,CPPFLAGS,[$2])="$ifelse([$2],,CPPFLAGS,[$2]) -I$cf_add_incdir"

		  cf_top_incdir=`echo "$cf_add_incdir" | sed -e 's%/include/.*$%/include%'`
		  test "$cf_top_incdir" = "$cf_add_incdir" && break
		  cf_add_incdir="$cf_top_incdir"
		else
		  break
		fi
	  else
		break
	  fi
	done
  done
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_LIB version: 2 updated: 2010/06/02 05:03:05
dnl ----------
dnl Add a library, used to enforce consistency.
dnl
dnl $1 = library to add, without the "-l"
dnl $2 = variable to update (default $LIBS)
AC_DEFUN([CF_ADD_LIB],[CF_ADD_LIBS(-l$1,ifelse($2,,LIBS,[$2]))])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_LIBDIR version: 11 updated: 2020/12/31 20:19:42
dnl -------------
dnl	Adds to the library-path
dnl
dnl	Some machines have trouble with multiple -L options.
dnl
dnl $1 is the (list of) directory(s) to add
dnl $2 is the optional name of the variable to update (default LDFLAGS)
dnl
AC_DEFUN([CF_ADD_LIBDIR],
[
if test -n "$1" ; then
	for cf_add_libdir in $1
	do
		if test "$cf_add_libdir" = /usr/lib ; then
			:
		elif test -d "$cf_add_libdir"
		then
			cf_have_libdir=no
			if test -n "$LDFLAGS$LIBS" ; then
				# a loop is needed to ensure we can add subdirs of existing dirs
				for cf_test_libdir in $LDFLAGS $LIBS ; do
					if test ".$cf_test_libdir" = ".-L$cf_add_libdir" ; then
						cf_have_libdir=yes; break
					fi
				done
			fi
			if test "$cf_have_libdir" = no ; then
				CF_VERBOSE(adding $cf_add_libdir to library-path)
				ifelse([$2],,LDFLAGS,[$2])="-L$cf_add_libdir $ifelse([$2],,LDFLAGS,[$2])"
			fi
		fi
	done
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_LIBS version: 3 updated: 2019/11/02 16:47:33
dnl -----------
dnl Add one or more libraries, used to enforce consistency.  Libraries are
dnl prepended to an existing list, since their dependencies are assumed to
dnl already exist in the list.
dnl
dnl $1 = libraries to add, with the "-l", etc.
dnl $2 = variable to update (default $LIBS)
AC_DEFUN([CF_ADD_LIBS],[
cf_add_libs="[$]ifelse($2,,LIBS,[$2])"
# reverse order
cf_add_0lib=
for cf_add_1lib in $1; do cf_add_0lib="$cf_add_1lib $cf_add_0lib"; done
# filter duplicates
for cf_add_1lib in $cf_add_0lib; do
	for cf_add_2lib in $cf_add_libs; do
		if test "x$cf_add_1lib" = "x$cf_add_2lib"; then
			cf_add_1lib=
			break
		fi
	done
	test -n "$cf_add_1lib" && cf_add_libs="$cf_add_1lib $cf_add_libs"
done
ifelse($2,,LIBS,[$2])="$cf_add_libs"
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_LIB_AFTER version: 3 updated: 2013/07/09 21:27:22
dnl ----------------
dnl Add a given library after another, e.g., following the one it satisfies a
dnl dependency for.
dnl
dnl $1 = the first library
dnl $2 = its dependency
AC_DEFUN([CF_ADD_LIB_AFTER],[
CF_VERBOSE(...before $LIBS)
LIBS=`echo "$LIBS" | sed -e "s/[[ 	]][[ 	]]*/ /g" -e "s%$1 %$1 $2 %" -e 's%  % %g'`
CF_VERBOSE(...after  $LIBS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ADD_SUBDIR_PATH version: 5 updated: 2020/12/31 20:19:42
dnl ------------------
dnl Append to a search-list for a nonstandard header/lib-file
dnl	$1 = the variable to return as result
dnl	$2 = the package name
dnl	$3 = the subdirectory, e.g., bin, include or lib
dnl $4 = the directory under which we will test for subdirectories
dnl $5 = a directory that we do not want $4 to match
AC_DEFUN([CF_ADD_SUBDIR_PATH],
[
test "x$4" != "x$5" && \
test -d "$4" && \
ifelse([$5],NONE,,[{ test -z "$5" || test "x$5" = xNONE || test "x$4" != "x$5"; } &&]) {
	test -n "$verbose" && echo "	... testing for $3-directories under $4"
	test -d "$4/$3" &&          $1="[$]$1 $4/$3"
	test -d "$4/$3/$2" &&       $1="[$]$1 $4/$3/$2"
	test -d "$4/$3/$2/$3" &&    $1="[$]$1 $4/$3/$2/$3"
	test -d "$4/$2/$3" &&       $1="[$]$1 $4/$2/$3"
	test -d "$4/$2/$3/$2" &&    $1="[$]$1 $4/$2/$3/$2"
}
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_APPEND_CFLAGS version: 3 updated: 2021/09/05 17:25:40
dnl ----------------
dnl Use CF_ADD_CFLAGS after first checking for potential redefinitions.
dnl $1 = flags to add
dnl $2 = if given makes this macro verbose.
define([CF_APPEND_CFLAGS],
[
for cf_add_cflags in $1
do
	case "x$cf_add_cflags" in
	(x-[[DU]]*)
		CF_REMOVE_CFLAGS($cf_add_cflags,CFLAGS,[$2])
		CF_REMOVE_CFLAGS($cf_add_cflags,CPPFLAGS,[$2])
		;;
	esac
	CF_ADD_CFLAGS([$cf_add_cflags],[$2])
done
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_APPEND_TEXT version: 1 updated: 2017/02/25 18:58:55
dnl --------------
dnl use this macro for appending text without introducing an extra blank at
dnl the beginning
define([CF_APPEND_TEXT],
[
	test -n "[$]$1" && $1="[$]$1 "
	$1="[$]{$1}$2"
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ARG_DISABLE version: 3 updated: 1999/03/30 17:24:31
dnl --------------
dnl Allow user to disable a normally-on option.
AC_DEFUN([CF_ARG_DISABLE],
[CF_ARG_OPTION($1,[$2],[$3],[$4],yes)])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ARG_ENABLE version: 3 updated: 1999/03/30 17:24:31
dnl -------------
dnl Allow user to enable a normally-off option.
AC_DEFUN([CF_ARG_ENABLE],
[CF_ARG_OPTION($1,[$2],[$3],[$4],no)])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ARG_OPTION version: 5 updated: 2015/05/10 19:52:14
dnl -------------
dnl Restricted form of AC_ARG_ENABLE that ensures user doesn't give bogus
dnl values.
dnl
dnl Parameters:
dnl $1 = option name
dnl $2 = help-string
dnl $3 = action to perform if option is not default
dnl $4 = action if perform if option is default
dnl $5 = default option value (either 'yes' or 'no')
AC_DEFUN([CF_ARG_OPTION],
[AC_ARG_ENABLE([$1],[$2],[test "$enableval" != ifelse([$5],no,yes,no) && enableval=ifelse([$5],no,no,yes)
	if test "$enableval" != "$5" ; then
ifelse([$3],,[    :]dnl
,[    $3]) ifelse([$4],,,[
	else
		$4])
	fi],[enableval=$5 ifelse([$4],,,[
	$4
])dnl
])])dnl
dnl ---------------------------------------------------------------------------
dnl CF_C11_NORETURN version: 3 updated: 2021/03/28 11:36:23
dnl ---------------
AC_DEFUN([CF_C11_NORETURN],
[
AC_MSG_CHECKING(if you want to use C11 _Noreturn feature)
CF_ARG_ENABLE(stdnoreturn,
	[  --enable-stdnoreturn    enable C11 _Noreturn feature for diagnostics],
	[enable_stdnoreturn=yes],
	[enable_stdnoreturn=no])
AC_MSG_RESULT($enable_stdnoreturn)

if test $enable_stdnoreturn = yes; then
AC_CACHE_CHECK([for C11 _Noreturn feature], cf_cv_c11_noreturn,
	[AC_TRY_COMPILE([
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
static _Noreturn void giveup(void) { exit(0); }
	],
	[if (feof(stdin)) giveup()],
	cf_cv_c11_noreturn=yes,
	cf_cv_c11_noreturn=no)
	])
else
	cf_cv_c11_noreturn=no,
fi

if test "$cf_cv_c11_noreturn" = yes; then
	AC_DEFINE(HAVE_STDNORETURN_H, 1,[Define if <stdnoreturn.h> header is available and working])
	AC_DEFINE_UNQUOTED(STDC_NORETURN,_Noreturn,[Define if C11 _Noreturn keyword is supported])
	HAVE_STDNORETURN_H=1
else
	HAVE_STDNORETURN_H=0
fi

AC_SUBST(HAVE_STDNORETURN_H)
AC_SUBST(STDC_NORETURN)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CC_ENV_FLAGS version: 10 updated: 2020/12/31 18:40:20
dnl ---------------
dnl Check for user's environment-breakage by stuffing CFLAGS/CPPFLAGS content
dnl into CC.  This will not help with broken scripts that wrap the compiler
dnl with options, but eliminates a more common category of user confusion.
dnl
dnl In particular, it addresses the problem of being able to run the C
dnl preprocessor in a consistent manner.
dnl
dnl Caveat: this also disallows blanks in the pathname for the compiler, but
dnl the nuisance of having inconsistent settings for compiler and preprocessor
dnl outweighs that limitation.
AC_DEFUN([CF_CC_ENV_FLAGS],
[
# This should have been defined by AC_PROG_CC
: "${CC:=cc}"

AC_MSG_CHECKING(\$CFLAGS variable)
case "x$CFLAGS" in
(*-[[IUD]]*)
	AC_MSG_RESULT(broken)
	AC_MSG_WARN(your environment uses the CFLAGS variable to hold CPPFLAGS options)
	cf_flags="$CFLAGS"
	CFLAGS=
	for cf_arg in $cf_flags
	do
		CF_ADD_CFLAGS($cf_arg)
	done
	;;
(*)
	AC_MSG_RESULT(ok)
	;;
esac

AC_MSG_CHECKING(\$CC variable)
case "$CC" in
(*[[\ \	]]-*)
	AC_MSG_RESULT(broken)
	AC_MSG_WARN(your environment uses the CC variable to hold CFLAGS/CPPFLAGS options)
	# humor him...
	cf_prog=`echo "$CC" | sed -e 's/	/ /g' -e 's/[[ ]]* / /g' -e 's/[[ ]]*[[ ]]-[[^ ]].*//'`
	cf_flags=`echo "$CC" | ${AWK:-awk} -v prog="$cf_prog" '{ printf("%s", [substr]([$]0,1+length(prog))); }'`
	CC="$cf_prog"
	for cf_arg in $cf_flags
	do
		case "x$cf_arg" in
		(x-[[IUDfgOW]]*)
			CF_ADD_CFLAGS($cf_arg)
			;;
		(*)
			CC="$CC $cf_arg"
			;;
		esac
	done
	CF_VERBOSE(resulting CC: '$CC')
	CF_VERBOSE(resulting CFLAGS: '$CFLAGS')
	CF_VERBOSE(resulting CPPFLAGS: '$CPPFLAGS')
	;;
(*)
	AC_MSG_RESULT(ok)
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CHECK_CACHE version: 13 updated: 2020/12/31 10:54:15
dnl --------------
dnl Check if we're accidentally using a cache from a different machine.
dnl Derive the system name, as a check for reusing the autoconf cache.
dnl
dnl If we've packaged config.guess and config.sub, run that (since it does a
dnl better job than uname).  Normally we'll use AC_CANONICAL_HOST, but allow
dnl an extra parameter that we may override, e.g., for AC_CANONICAL_SYSTEM
dnl which is useful in cross-compiles.
dnl
dnl Note: we would use $ac_config_sub, but that is one of the places where
dnl autoconf 2.5x broke compatibility with autoconf 2.13
AC_DEFUN([CF_CHECK_CACHE],
[
if test -f "$srcdir/config.guess" || test -f "$ac_aux_dir/config.guess" ; then
	ifelse([$1],,[AC_CANONICAL_HOST],[$1])
	system_name="$host_os"
else
	system_name="`(uname -s -r) 2>/dev/null`"
	if test -z "$system_name" ; then
		system_name="`(hostname) 2>/dev/null`"
	fi
fi
test -n "$system_name" && AC_DEFINE_UNQUOTED(SYSTEM_NAME,"$system_name",[Define to the system name.])
AC_CACHE_VAL(cf_cv_system_name,[cf_cv_system_name="$system_name"])

test -z "$system_name" && system_name="$cf_cv_system_name"
test -n "$cf_cv_system_name" && AC_MSG_RESULT(Configuring for $cf_cv_system_name)

if test ".$system_name" != ".$cf_cv_system_name" ; then
	AC_MSG_RESULT(Cached system name ($system_name) does not agree with actual ($cf_cv_system_name))
	AC_MSG_ERROR("Please remove config.cache and try again.")
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CHECK_CFLAGS version: 4 updated: 2021/01/02 19:22:58
dnl ---------------
dnl Conditionally add to $CFLAGS and $CPPFLAGS values which are derived from
dnl a build-configuration such as imake.  These have the pitfall that they
dnl often contain compiler-specific options which we cannot use, mixed with
dnl preprocessor options that we usually can.
AC_DEFUN([CF_CHECK_CFLAGS],
[
CF_VERBOSE(checking additions to CFLAGS)
cf_check_cflags="$CFLAGS"
cf_check_cppflags="$CPPFLAGS"
CF_ADD_CFLAGS($1,yes)
if test "x$cf_check_cflags" != "x$CFLAGS" ; then
AC_TRY_LINK([#include <stdio.h>],[printf("Hello world");],,
	[CF_VERBOSE(test-compile failed.  Undoing change to \$CFLAGS)
	 if test "x$cf_check_cppflags" != "x$CPPFLAGS" ; then
		 CF_VERBOSE(but keeping change to \$CPPFLAGS)
	 fi
	 CFLAGS="$cf_check_cflags"])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CHECK_CURSES_LIB version: 4 updated: 2021/09/05 17:25:40
dnl -------------------
dnl $1 = nominal library name, used also for header lookup
dnl $2 = suffix to append to library name
dnl $3 = function to check for using AC_CHECK_LIB
dnl $4 = optional parameter list for $3
AC_DEFUN([CF_CHECK_CURSES_LIB],
[
AC_REQUIRE([CF_PKG_CONFIG])

cf_have_curses_lib=no

: ${NCURSES_CONFIG_PKG:=none}
if test "x${NCURSES_CONFIG_PKG}" = xnone; then
	:
elif test "x${PKG_CONFIG:=none}" != xnone; then
	AC_MSG_CHECKING(pkg-config for $1$2)
	if "$PKG_CONFIG" --exists "$1$2" ; then
		AC_MSG_RESULT(yes)

		AC_MSG_CHECKING(if the $1$2 package files work)

		cf_save_CFLAGS="$CFLAGS"
		cf_save_CPPFLAGS="$CPPFLAGS"
		cf_save_LIBS="$LIBS"

		CF_APPEND_CFLAGS(`$PKG_CONFIG --cflags "$1$2"`)
		CF_ADD_LIBS(`$PKG_CONFIG --libs "$1$2"`)

		AC_TRY_LINK([#include <$1.h>],
			[(void) $3 ( ]ifelse([$4],,,[[$4]])[ );],
			[AC_TRY_RUN([#include <$1.h>
				int main(void)
				{ (void) $3 ( ]ifelse([$4],,,[[$4]])[ ); return 0; }],
				[cf_have_curses_lib=yes],
				[cf_have_curses_lib=no],
				[cf_have_curses_lib=maybe])],
			[cf_have_curses_lib=no])
		AC_MSG_RESULT($cf_have_curses_lib)
		test "$cf_have_curses_lib" = maybe && cf_have_curses_lib=yes
		if test "$cf_have_curses_lib" = "yes"
		then
			CF_UPPER(cf_upper,have_lib$1)
			AC_DEFINE_UNQUOTED($cf_upper,1)
		else
			CFLAGS="$cf_save_CFLAGS"
			CPPFLAGS="$cf_save_CPPFLAGS"
			LIBS="$cf_save_LIBS"
		fi
	fi
fi
if test "$cf_have_curses_lib" = no; then
	AC_CHECK_LIB($1$2,$3,[
		CF_UPPER(cf_upper,have_lib$1)
		CF_ADD_LIBS(-l$1$2)
		AC_DEFINE_UNQUOTED($cf_upper,1)])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CLANG_COMPILER version: 8 updated: 2021/01/01 13:31:04
dnl -----------------
dnl Check if the given compiler is really clang.  clang's C driver defines
dnl __GNUC__ (fooling the configure script into setting $GCC to yes) but does
dnl not ignore some gcc options.
dnl
dnl This macro should be run "soon" after AC_PROG_CC or AC_PROG_CPLUSPLUS, to
dnl ensure that it is not mistaken for gcc/g++.  It is normally invoked from
dnl the wrappers for gcc and g++ warnings.
dnl
dnl $1 = GCC (default) or GXX
dnl $2 = CLANG_COMPILER (default)
dnl $3 = CFLAGS (default) or CXXFLAGS
AC_DEFUN([CF_CLANG_COMPILER],[
ifelse([$2],,CLANG_COMPILER,[$2])=no

if test "$ifelse([$1],,[$1],GCC)" = yes ; then
	AC_MSG_CHECKING(if this is really Clang ifelse([$1],GXX,C++,C) compiler)
	cf_save_CFLAGS="$ifelse([$3],,CFLAGS,[$3])"
	AC_TRY_COMPILE([],[
#ifdef __clang__
#else
make an error
#endif
],[ifelse([$2],,CLANG_COMPILER,[$2])=yes
],[])
	ifelse([$3],,CFLAGS,[$3])="$cf_save_CFLAGS"
	AC_MSG_RESULT($ifelse([$2],,CLANG_COMPILER,[$2]))
fi

CLANG_VERSION=none

if test "x$ifelse([$2],,CLANG_COMPILER,[$2])" = "xyes" ; then
	case "$CC" in
	(c[[1-9]][[0-9]]|*/c[[1-9]][[0-9]])
		AC_MSG_WARN(replacing broken compiler alias $CC)
		CFLAGS="$CFLAGS -std=`echo "$CC" | sed -e 's%.*/%%'`"
		CC=clang
		;;
	esac

	AC_MSG_CHECKING(version of $CC)
	CLANG_VERSION="`$CC --version 2>/dev/null | sed -e '2,$d' -e 's/^.*(CLANG[[^)]]*) //' -e 's/^.*(Debian[[^)]]*) //' -e 's/^[[^0-9.]]*//' -e 's/[[^0-9.]].*//'`"
	test -z "$CLANG_VERSION" && CLANG_VERSION=unknown
	AC_MSG_RESULT($CLANG_VERSION)

	for cf_clang_opt in \
		-Qunused-arguments \
		-Wno-error=implicit-function-declaration
	do
		AC_MSG_CHECKING(if option $cf_clang_opt works)
		cf_save_CFLAGS="$CFLAGS"
		CFLAGS="$CFLAGS $cf_clang_opt"
		AC_TRY_LINK([
			#include <stdio.h>],[
			printf("hello!\\n");],[
			cf_clang_optok=yes],[
			cf_clang_optok=no])
		AC_MSG_RESULT($cf_clang_optok)
		CFLAGS="$cf_save_CFLAGS"
		if test "$cf_clang_optok" = yes; then
			CF_VERBOSE(adding option $cf_clang_opt)
			CF_APPEND_TEXT(CFLAGS,$cf_clang_opt)
		fi
	done
fi
])
dnl ---------------------------------------------------------------------------
dnl CF_CONST_X_STRING version: 7 updated: 2021/06/07 17:39:17
dnl -----------------
dnl The X11R4-X11R6 Xt specification uses an ambiguous String type for most
dnl character-strings.
dnl
dnl It is ambiguous because the specification accommodated the pre-ANSI
dnl compilers bundled by more than one vendor in lieu of providing a standard C
dnl compiler other than by costly add-ons.  Because of this, the specification
dnl did not take into account the use of const for telling the compiler that
dnl string literals would be in readonly memory.
dnl
dnl As a workaround, one could (starting with X11R5) define XTSTRINGDEFINES, to
dnl let the compiler decide how to represent Xt's strings which were #define'd.
dnl That does not solve the problem of using the block of Xt's strings which
dnl are compiled into the library (and is less efficient than one might want).
dnl
dnl Xt specification 7 introduces the _CONST_X_STRING symbol which is used both
dnl when compiling the library and compiling using the library, to tell the
dnl compiler that String is const.
AC_DEFUN([CF_CONST_X_STRING],
[
AC_REQUIRE([AC_PATH_XTRA])

CF_SAVE_XTRA_FLAGS([CF_CONST_X_STRING])

AC_TRY_COMPILE(
[
#include <stdlib.h>
#include <X11/Intrinsic.h>
],
[String foo = malloc(1); free((void*)foo)],[

AC_CACHE_CHECK(for X11/Xt const-feature,cf_cv_const_x_string,[
	AC_TRY_COMPILE(
		[
#define _CONST_X_STRING	/* X11R7.8 (perhaps) */
#undef  XTSTRINGDEFINES	/* X11R5 and later */
#include <stdlib.h>
#include <X11/Intrinsic.h>
		],[String foo = malloc(1); *foo = 0],[
			cf_cv_const_x_string=no
		],[
			cf_cv_const_x_string=yes
		])
])

CF_RESTORE_XTRA_FLAGS([CF_CONST_X_STRING])

case "$cf_cv_const_x_string" in
(no)
	CF_APPEND_TEXT(CPPFLAGS,-DXTSTRINGDEFINES)
	;;
(*)
	CF_APPEND_TEXT(CPPFLAGS,-D_CONST_X_STRING)
	;;
esac

])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_ACS_MAP version: 8 updated: 2021/01/04 19:45:09
dnl -----------------
dnl Check for likely values of acs_map[]:
AC_DEFUN([CF_CURSES_ACS_MAP],
[
AC_REQUIRE([CF_NCURSES_WRAP_PREFIX])dnl
AC_CACHE_CHECK(for alternate character set array, cf_cv_curses_acs_map,[
cf_cv_curses_acs_map=unknown
for name in acs_map _acs_map __acs_map ${NCURSES_WRAP_PREFIX}acs_map
do
AC_TRY_LINK([
#include <${cf_cv_ncurses_header:-curses.h}>
],[
${name}['k'] = ACS_PLUS
],[cf_cv_curses_acs_map=$name; break])
done
])

test "$cf_cv_curses_acs_map" != unknown && AC_DEFINE_UNQUOTED(CURSES_ACS_ARRAY,$cf_cv_curses_acs_map,[Define as needed to override ncurses prefix _nc_])
])
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_CHECK_DATA version: 10 updated: 2021/01/04 19:45:09
dnl --------------------
dnl Check if curses.h defines the given data/variable.
dnl Use this after CF_NCURSES_CONFIG or CF_CURSES_CONFIG.
dnl
dnl $1 = data item(s) to check for
dnl $2 = action on success, e.g., "break" to quit checking a series of choices
AC_DEFUN([CF_CURSES_CHECK_DATA],
[
for cf_data in $1
do
AC_MSG_CHECKING(for data $cf_data declaration in ${cf_cv_ncurses_header:-curses.h})

AC_TRY_COMPILE(CF__CURSES_HEAD,
CF__CURSES_DATA(foo,$cf_data)
,[cf_result=yes
],[cf_result=no])
AC_MSG_RESULT($cf_result)

if test "$cf_result" = yes ; then
	CF_UPPER(cf_result,have_curses_data_$cf_data)
	AC_DEFINE_UNQUOTED($cf_result)
	ifelse($2,,,[$2])
else
	AC_MSG_CHECKING(for data $cf_data in library)
	# BSD linkers insist on making weak linkage, but resolve at runtime.
	AC_TRY_RUN(CF__CURSES_HEAD
[
extern char $cf_data;
int main(void)
{
	]CF__CURSES_DATA(foo,$cf_data)[
	${cf_cv_main_return:-return}(foo == 0);
}],[cf_result=yes
],[cf_result=no],[
	# cross-compiling
	AC_TRY_LINK(CF__CURSES_HEAD
[extern char $cf_data;],[
	do {
		]CF__CURSES_DATA(foo,$cf_data)[
		${cf_cv_main_return:-return}(foo == 0);
	} while (0)
],[cf_result=yes],[cf_result=no])
])
	AC_MSG_RESULT($cf_result)
	if test "$cf_result" = yes ; then
		CF_UPPER(cf_result,decl_curses_data_$cf_data)
		AC_DEFINE_UNQUOTED($cf_result)
		# do not exit loop here, since we prefer system's declarations
	fi
fi
done
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_CHECK_TYPE version: 5 updated: 2021/01/04 19:45:09
dnl --------------------
dnl Check if curses.h defines the given type
AC_DEFUN([CF_CURSES_CHECK_TYPE],
[
AC_MSG_CHECKING(for type $1 in ${cf_cv_ncurses_header:-curses.h})
AC_TRY_COMPILE([
#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif
#include <${cf_cv_ncurses_header:-curses.h}>],[
$1 foo
],cf_result=yes,cf_result=no)
AC_MSG_RESULT($cf_result)
if test "$cf_result" = yes ; then
	CF_UPPER(cf_result,have_type_$1)
	AC_DEFINE_UNQUOTED($cf_result,1,[Define to 1 if we have type $1])
else
	AC_DEFINE_UNQUOTED($1,$2,[Define to appropriate type if $1 is not declared])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_CONFIG version: 2 updated: 2006/10/29 11:06:27
dnl ----------------
dnl Tie together the configure-script macros for curses.  It may be ncurses,
dnl but unless asked, we do not make a special search for ncurses.  However,
dnl still check for the ncurses version number, for use in other macros.
AC_DEFUN([CF_CURSES_CONFIG],
[
CF_CURSES_CPPFLAGS
CF_NCURSES_VERSION
CF_CURSES_LIBS
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_CPPFLAGS version: 14 updated: 2021/01/02 09:31:20
dnl ------------------
dnl Look for the curses headers.
AC_DEFUN([CF_CURSES_CPPFLAGS],[

AC_CACHE_CHECK(for extra include directories,cf_cv_curses_incdir,[
cf_cv_curses_incdir=no
case "$host_os" in
(hpux10.*)
	if test "x$cf_cv_screen" = "xcurses_colr"
	then
		test -d /usr/include/curses_colr && \
		cf_cv_curses_incdir="-I/usr/include/curses_colr"
	fi
	;;
(sunos3*|sunos4*)
	if test "x$cf_cv_screen" = "xcurses_5lib"
	then
		test -d /usr/5lib && \
		test -d /usr/5include && \
		cf_cv_curses_incdir="-I/usr/5include"
	fi
	;;
esac
])
if test "$cf_cv_curses_incdir" != no
then
	CF_APPEND_TEXT(CPPFLAGS,$cf_cv_curses_incdir)
fi

CF_CURSES_HEADER
CF_TERM_HEADER
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_FUNCS version: 20 updated: 2020/12/31 20:19:42
dnl ---------------
dnl Curses-functions are a little complicated, since a lot of them are macros.
AC_DEFUN([CF_CURSES_FUNCS],
[
AC_REQUIRE([CF_CURSES_CPPFLAGS])dnl
AC_REQUIRE([CF_XOPEN_CURSES])
AC_REQUIRE([CF_CURSES_TERM_H])
AC_REQUIRE([CF_CURSES_UNCTRL_H])
for cf_func in $1
do
	CF_UPPER(cf_tr_func,$cf_func)
	AC_MSG_CHECKING(for ${cf_func})
	CF_MSG_LOG(${cf_func})
	AC_CACHE_VAL(cf_cv_func_$cf_func,[
		eval cf_result='$ac_cv_func_'$cf_func
		if test ".$cf_result" != ".no"; then
			AC_TRY_LINK(CF__CURSES_HEAD,
			[
#ifndef ${cf_func}
long foo = (long)(&${cf_func});
fprintf(stderr, "testing linkage of $cf_func:%p\\n", (void *)foo);
if (foo + 1234L > 5678L)
	${cf_cv_main_return:-return}(foo != 0);
#endif
			],
			[cf_result=yes],
			[cf_result=no])
		fi
		eval 'cf_cv_func_'$cf_func'="$cf_result"'
	])
	# use the computed/retrieved cache-value:
	eval 'cf_result=$cf_cv_func_'$cf_func
	AC_MSG_RESULT($cf_result)
	if test "$cf_result" != no; then
		AC_DEFINE_UNQUOTED(HAVE_${cf_tr_func})
	fi
done
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_HEADER version: 5 updated: 2015/04/23 20:35:30
dnl ----------------
dnl Find a "curses" header file, e.g,. "curses.h", or one of the more common
dnl variations of ncurses' installs.
dnl
dnl $1 = ncurses when looking for ncurses, or is empty
AC_DEFUN([CF_CURSES_HEADER],[
AC_CACHE_CHECK(if we have identified curses headers,cf_cv_ncurses_header,[
cf_cv_ncurses_header=none
for cf_header in \
	ncurses.h ifelse($1,,,[$1/ncurses.h]) \
	curses.h ifelse($1,,,[$1/curses.h]) ifelse($1,,[ncurses/ncurses.h ncurses/curses.h])
do
AC_TRY_COMPILE([#include <${cf_header}>],
	[initscr(); tgoto("?", 0,0)],
	[cf_cv_ncurses_header=$cf_header; break],[])
done
])

if test "$cf_cv_ncurses_header" = none ; then
	AC_MSG_ERROR(No curses header-files found)
fi

# cheat, to get the right #define's for HAVE_NCURSES_H, etc.
AC_CHECK_HEADERS($cf_cv_ncurses_header)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_LIBS version: 44 updated: 2021/01/02 09:31:20
dnl --------------
dnl Look for the curses libraries.  Older curses implementations may require
dnl termcap/termlib to be linked as well.  Call CF_CURSES_CPPFLAGS first.
AC_DEFUN([CF_CURSES_LIBS],[

AC_REQUIRE([CF_CURSES_CPPFLAGS])dnl
AC_MSG_CHECKING(if we have identified curses libraries)
AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
	[initscr(); tgoto("?", 0,0)],
	cf_result=yes,
	cf_result=no)
AC_MSG_RESULT($cf_result)

if test "$cf_result" = no ; then
case "$host_os" in
(freebsd*)
	AC_CHECK_LIB(mytinfo,tgoto,[CF_ADD_LIBS(-lmytinfo)])
	;;
(hpux10.*)
	# Looking at HPUX 10.20, the Hcurses library is the oldest (1997), cur_colr
	# next (1998), and xcurses "newer" (2000).  There is no header file for
	# Hcurses; the subdirectory curses_colr has the headers (curses.h and
	# term.h) for cur_colr
	if test "x$cf_cv_screen" = "xcurses_colr"
	then
		AC_CHECK_LIB(cur_colr,initscr,[
			CF_ADD_LIBS(-lcur_colr)
			ac_cv_func_initscr=yes
			],[
		AC_CHECK_LIB(Hcurses,initscr,[
			# HP's header uses __HP_CURSES, but user claims _HP_CURSES.
			CF_ADD_LIBS(-lHcurses)
			CF_APPEND_TEXT(CPPFLAGS,-D__HP_CURSES -D_HP_CURSES)
			ac_cv_func_initscr=yes
			])])
	fi
	;;
(linux*)
	case `arch 2>/dev/null` in
	(x86_64)
		if test -d /lib64
		then
			CF_ADD_LIBDIR(/lib64)
		else
			CF_ADD_LIBDIR(/lib)
		fi
		;;
	(*)
		CF_ADD_LIBDIR(/lib)
		;;
	esac
	;;
(sunos3*|sunos4*)
	if test "x$cf_cv_screen" = "xcurses_5lib"
	then
		if test -d /usr/5lib ; then
			CF_ADD_LIBDIR(/usr/5lib)
			CF_ADD_LIBS(-lcurses -ltermcap)
		fi
	fi
	ac_cv_func_initscr=yes
	;;
esac

if test ".$ac_cv_func_initscr" != .yes ; then
	cf_save_LIBS="$LIBS"

	if test ".${cf_cv_ncurses_version:-no}" != .no
	then
		cf_check_list="ncurses curses cursesX"
	else
		cf_check_list="cursesX curses ncurses"
	fi

	# Check for library containing tgoto.  Do this before curses library
	# because it may be needed to link the test-case for initscr.
	if test "x$cf_term_lib" = x
	then
		AC_CHECK_FUNC(tgoto,[cf_term_lib=predefined],[
			for cf_term_lib in $cf_check_list otermcap termcap tinfo termlib unknown
			do
				AC_CHECK_LIB($cf_term_lib,tgoto,[
					: "${cf_nculib_root:=$cf_term_lib}"
					break
				])
			done
		])
	fi

	# Check for library containing initscr
	test "$cf_term_lib" != predefined && test "$cf_term_lib" != unknown && LIBS="-l$cf_term_lib $cf_save_LIBS"
	if test "x$cf_curs_lib" = x
	then
		for cf_curs_lib in $cf_check_list xcurses jcurses pdcurses unknown
		do
			LIBS="-l$cf_curs_lib $cf_save_LIBS"
			if test "$cf_term_lib" = unknown || test "$cf_term_lib" = "$cf_curs_lib" ; then
				AC_MSG_CHECKING(if we can link with $cf_curs_lib library)
				AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
					[initscr()],
					[cf_result=yes],
					[cf_result=no])
				AC_MSG_RESULT($cf_result)
				test "$cf_result" = yes && break
			elif test "$cf_curs_lib" = "$cf_term_lib" ; then
				cf_result=no
			elif test "$cf_term_lib" != predefined ; then
				AC_MSG_CHECKING(if we need both $cf_curs_lib and $cf_term_lib libraries)
				AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
					[initscr(); tgoto((char *)0, 0, 0);],
					[cf_result=no],
					[
					LIBS="-l$cf_curs_lib -l$cf_term_lib $cf_save_LIBS"
					AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
						[initscr()],
						[cf_result=yes],
						[cf_result=error])
					])
				AC_MSG_RESULT($cf_result)
				test "$cf_result" != error && break
			fi
		done
	fi
	test "$cf_curs_lib" = unknown && AC_MSG_ERROR(no curses library found)
fi
fi

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_TERM_H version: 15 updated: 2021/01/02 09:31:20
dnl ----------------
dnl SVr4 curses should have term.h as well (where it puts the definitions of
dnl the low-level interface).  This may not be true in old/broken implementations,
dnl as well as in misconfigured systems (e.g., gcc configured for Solaris 2.4
dnl running with Solaris 2.5.1).
AC_DEFUN([CF_CURSES_TERM_H],
[
AC_REQUIRE([CF_CURSES_CPPFLAGS])dnl

AC_CACHE_CHECK(for term.h, cf_cv_term_header,[

# If we found <ncurses/curses.h>, look for <ncurses/term.h>, but always look
# for <term.h> if we do not find the variant.

cf_header_list="term.h ncurses/term.h ncursesw/term.h"

case "${cf_cv_ncurses_header:-curses.h}" in
(*/*)
	cf_header_item=`echo "${cf_cv_ncurses_header:-curses.h}" | sed -e 's%\..*%%' -e 's%/.*%/%'`term.h
	cf_header_list="$cf_header_item $cf_header_list"
	;;
esac

for cf_header in $cf_header_list
do
	AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>
#include <${cf_header}>],
	[WINDOW *x; (void)x],
	[cf_cv_term_header=$cf_header
	 break],
	[cf_cv_term_header=no])
done

case "$cf_cv_term_header" in
(no)
	# If curses is ncurses, some packagers still mess it up by trying to make
	# us use GNU termcap.  This handles the most common case.
	for cf_header in ncurses/term.h ncursesw/term.h
	do
		AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>
#ifdef NCURSES_VERSION
#include <${cf_header}>
#else
make an error
#endif],
			[WINDOW *x; (void)x],
			[cf_cv_term_header=$cf_header
			 break],
			[cf_cv_term_header=no])
	done
	;;
esac
])

case "$cf_cv_term_header" in
(term.h)
	AC_DEFINE(HAVE_TERM_H,1,[Define to 1 if we have term.h])
	;;
(ncurses/term.h)
	AC_DEFINE(HAVE_NCURSES_TERM_H,1,[Define to 1 if we have ncurses/term.h])
	;;
(ncursesw/term.h)
	AC_DEFINE(HAVE_NCURSESW_TERM_H,1,[Define to 1 if we have ncursesw/term.h])
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_UNCTRL_H version: 8 updated: 2021/01/02 09:31:20
dnl ------------------
dnl Any X/Open curses implementation must have unctrl.h, but ncurses packages
dnl may put it in a subdirectory (along with ncurses' other headers, of
dnl course).  Packages which put the headers in inconsistent locations are
dnl broken).
AC_DEFUN([CF_CURSES_UNCTRL_H],
[
AC_REQUIRE([CF_CURSES_CPPFLAGS])dnl

AC_CACHE_CHECK(for unctrl.h, cf_cv_unctrl_header,[

# If we found <ncurses/curses.h>, look for <ncurses/unctrl.h>, but always look
# for <unctrl.h> if we do not find the variant.

cf_header_list="unctrl.h ncurses/unctrl.h ncursesw/unctrl.h"

case "${cf_cv_ncurses_header:-curses.h}" in
(*/*)
	cf_header_item=`echo "${cf_cv_ncurses_header:-curses.h}" | sed -e 's%\..*%%' -e 's%/.*%/%'`unctrl.h
	cf_header_list="$cf_header_item $cf_header_list"
	;;
esac

for cf_header in $cf_header_list
do
	AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>
#include <${cf_header}>],
	[WINDOW *x; (void)x],
	[cf_cv_unctrl_header=$cf_header
	 break],
	[cf_cv_unctrl_header=no])
done
])

case "$cf_cv_unctrl_header" in
(no)
	AC_MSG_WARN(unctrl.h header not found)
	;;
esac

case "$cf_cv_unctrl_header" in
(unctrl.h)
	AC_DEFINE(HAVE_UNCTRL_H,1,[Define to 1 if we have unctrl.h])
	;;
(ncurses/unctrl.h)
	AC_DEFINE(HAVE_NCURSES_UNCTRL_H,1,[Define to 1 if we have ncurses/unctrl.h])
	;;
(ncursesw/unctrl.h)
	AC_DEFINE(HAVE_NCURSESW_UNCTRL_H,1,[Define to 1 if we have ncursesw/unctrl.h])
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_WACS_MAP version: 8 updated: 2021/01/04 19:45:09
dnl ------------------
dnl Check for likely values of wacs_map[].
AC_DEFUN([CF_CURSES_WACS_MAP],
[
AC_CACHE_CHECK(for wide alternate character set array, cf_cv_curses_wacs_map,[
	cf_cv_curses_wacs_map=unknown
	for name in wacs_map _wacs_map __wacs_map _nc_wacs _wacs_char
	do
	AC_TRY_LINK([
#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif
#include <${cf_cv_ncurses_header:-curses.h}>],
	[void *foo = &(${name}['k']); (void)foo],
	[cf_cv_curses_wacs_map=$name
	 break])
	done])

test "$cf_cv_curses_wacs_map" != unknown && AC_DEFINE_UNQUOTED(CURSES_WACS_ARRAY,$cf_cv_curses_wacs_map,[Define to name of (n)curses wide-character array])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_CURSES_WACS_SYMBOLS version: 4 updated: 2021/01/04 19:45:09
dnl ----------------------
dnl Do a check to see if the WACS_xxx constants are defined compatibly with
dnl X/Open Curses.  In particular, NetBSD's implementation of the WACS_xxx
dnl constants is broken since those constants do not point to cchar_t's.
AC_DEFUN([CF_CURSES_WACS_SYMBOLS],
[
AC_REQUIRE([CF_CURSES_WACS_MAP])

AC_CACHE_CHECK(for wide alternate character constants, cf_cv_curses_wacs_symbols,[
cf_cv_curses_wacs_symbols=no
if test "$cf_cv_curses_wacs_map" != unknown
then
	AC_TRY_LINK([
#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif
#include <${cf_cv_ncurses_header:-curses.h}>],
	[cchar_t *foo = WACS_PLUS;
	 ${cf_cv_curses_wacs_map}['k'] = *WACS_PLUS; (void)foo],
	[cf_cv_curses_wacs_symbols=yes])
else
	AC_TRY_LINK([
#ifndef _XOPEN_SOURCE_EXTENDED
#define _XOPEN_SOURCE_EXTENDED
#endif
#include <${cf_cv_ncurses_header:-curses.h}>],
	[cchar_t *foo = WACS_PLUS; (void)foo],
	[cf_cv_curses_wacs_symbols=yes])
fi
])

test "$cf_cv_curses_wacs_symbols" != no && AC_DEFINE(CURSES_WACS_SYMBOLS,1,[Define to 1 if (n)curses supports wide-character WACS_ symbols])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_DIRNAME version: 5 updated: 2020/12/31 20:19:42
dnl ----------
dnl "dirname" is not portable, so we fake it with a shell script.
AC_DEFUN([CF_DIRNAME],[$1=`echo "$2" | sed -e 's%/[[^/]]*$%%'`])dnl
dnl ---------------------------------------------------------------------------
dnl CF_DISABLE_ECHO version: 14 updated: 2021/09/04 06:35:04
dnl ---------------
dnl You can always use "make -n" to see the actual options, but it is hard to
dnl pick out/analyze warning messages when the compile-line is long.
dnl
dnl Sets:
dnl	ECHO_LT - symbol to control if libtool is verbose
dnl	ECHO_LD - symbol to prefix "cc -o" lines
dnl	RULE_CC - symbol to put before implicit "cc -c" lines (e.g., .c.o)
dnl	SHOW_CC - symbol to put before explicit "cc -c" lines
dnl	ECHO_CC - symbol to put before any "cc" line
dnl
AC_DEFUN([CF_DISABLE_ECHO],[
AC_MSG_CHECKING(if you want to see long compiling messages)
CF_ARG_DISABLE(echo,
	[  --disable-echo          do not display "compiling" commands],
	[
	ECHO_LT='--silent'
	ECHO_LD='@echo linking [$]@;'
	RULE_CC='@echo compiling [$]<'
	SHOW_CC='@echo compiling [$]@'
	ECHO_CC='@'
],[
	ECHO_LT=''
	ECHO_LD=''
	RULE_CC=''
	SHOW_CC=''
	ECHO_CC=''
])
AC_MSG_RESULT($enableval)
AC_SUBST(ECHO_LT)
AC_SUBST(ECHO_LD)
AC_SUBST(RULE_CC)
AC_SUBST(SHOW_CC)
AC_SUBST(ECHO_CC)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_DISABLE_LEAKS version: 9 updated: 2021/04/03 16:41:50
dnl ----------------
dnl Combine no-leak checks with the libraries or tools that are used for the
dnl checks.
AC_DEFUN([CF_DISABLE_LEAKS],[

AC_REQUIRE([CF_WITH_DMALLOC])
AC_REQUIRE([CF_WITH_DBMALLOC])
AC_REQUIRE([CF_WITH_VALGRIND])

AC_MSG_CHECKING(if you want to perform memory-leak testing)
AC_ARG_ENABLE(leaks,
	[  --disable-leaks         test: free permanent memory, analyze leaks],
	[enable_leaks=$enableval],
	[enable_leaks=yes])
dnl with_no_leaks is more readable...
if test "x$enable_leaks" = xno; then with_no_leaks=yes; else with_no_leaks=no; fi
AC_MSG_RESULT($with_no_leaks)

if test "$enable_leaks" = no ; then
	AC_DEFINE(NO_LEAKS,1,[Define to 1 if you want to perform memory-leak testing.])
	AC_DEFINE(YY_NO_LEAKS,1,[Define to 1 if you want to perform memory-leak testing.])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_DISABLE_RPATH_HACK version: 3 updated: 2021/01/05 20:14:44
dnl ---------------------
dnl The rpath-hack makes it simpler to build programs, particularly with the
dnl *BSD ports which may have essential libraries in unusual places.  But it
dnl can interfere with building an executable for the base system.  Use this
dnl option in that case.
AC_DEFUN([CF_DISABLE_RPATH_HACK],
[
AC_MSG_CHECKING(if rpath-hack should be disabled)
CF_ARG_DISABLE(rpath-hack,
	[  --disable-rpath-hack    don't add rpath options for additional libraries],
	[enable_rpath_hack=no],
	[enable_rpath_hack=yes])
dnl TODO - drop cf_disable_rpath_hack
if test "x$enable_rpath_hack" = xno; then cf_disable_rpath_hack=yes; else cf_disable_rpath_hack=no; fi
AC_MSG_RESULT($cf_disable_rpath_hack)

if test "$enable_rpath_hack" = yes ; then
	CF_RPATH_HACK
fi
])
dnl ---------------------------------------------------------------------------
dnl CF_ENABLE_STRING_HACKS version: 6 updated: 2021/01/05 19:23:48
dnl ----------------------
dnl On a few platforms, the compiler and/or loader nags with untruthful
dnl comments stating that "most" uses of strcat/strcpy/sprintf are incorrect,
dnl and implying that most uses of the recommended alternatives are correct.
dnl
dnl Factually speaking, no one has actually counted the number of uses of these
dnl functions versus the total of incorrect uses.  Samples of a few thousand
dnl instances are meaningless compared to the hundreds of millions of lines of
dnl existing C code.
dnl
dnl strlcat/strlcpy are (as of 2012) non-standard, and are available on some
dnl platforms, in implementations of varying quality.  Likewise, snprintf is
dnl standard - but evolved through phases, and older implementations are likely
dnl to yield surprising results, as documented in manpages on various systems.
AC_DEFUN([CF_ENABLE_STRING_HACKS],
[
AC_MSG_CHECKING(if you want to work around bogus compiler/loader warnings)
AC_ARG_ENABLE(string-hacks,
	[  --enable-string-hacks   work around bogus compiler/loader warnings],
	[enable_string_hacks=$enableval],
	[enable_string_hacks=no])
AC_MSG_RESULT($enable_string_hacks)

if test "x$enable_string_hacks" = "xyes"; then
 	AC_DEFINE(USE_STRING_HACKS,1,[Define to 1 to work around bogus compiler/loader warnings])
	AC_MSG_WARN(enabling string-hacks to work around bogus compiler/loader warnings)
	AC_CHECK_FUNC(strlcat,[
		AC_DEFINE(HAVE_STRLCAT,1,[Define to 1 if we have strlcat function])
		],[
		AC_CHECK_LIB(bsd,strlcat,[
			CF_ADD_LIB(bsd)
			AC_CHECK_HEADERS(bsd/string.h)
			AC_DEFINE(HAVE_STRLCAT,1,[Define to 1 if we have strlcat function])
			])
		])
	AC_CHECK_FUNCS( strlcpy snprintf )
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_ENABLE_WARNINGS version: 9 updated: 2021/01/05 19:40:50
dnl ------------------
dnl Configure-option to enable gcc warnings
dnl
dnl $1 = extra options to add, if supported
dnl $2 = option for checking attributes.  By default, this is done when
dnl      warnings are enabled.  For other values:
dnl      yes: always do this, e.g., to use in generated library-headers
dnl      no: never do this
AC_DEFUN([CF_ENABLE_WARNINGS],[
if test "$GCC" = yes || test "$GXX" = yes
then
CF_FIX_WARNINGS(CFLAGS)
CF_FIX_WARNINGS(CPPFLAGS)
CF_FIX_WARNINGS(LDFLAGS)
AC_MSG_CHECKING(if you want to turn on gcc warnings)
CF_ARG_ENABLE(warnings,
	[  --enable-warnings       test: turn on gcc compiler warnings],
	[enable_warnings=yes],
	[enable_warnings=no])
AC_MSG_RESULT($enable_warnings)
if test "$enable_warnings" = "yes"
then
	ifelse($2,,[CF_GCC_ATTRIBUTES])
	CF_GCC_WARNINGS($1)
fi
ifelse($2,yes,[CF_GCC_ATTRIBUTES])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_FIND_LIBRARY version: 11 updated: 2021/01/02 09:31:20
dnl ---------------
dnl Look for a non-standard library, given parameters for AC_TRY_LINK.  We
dnl prefer a standard location, and use -L options only if we do not find the
dnl library in the standard library location(s).
dnl	$1 = library name
dnl	$2 = library class, usually the same as library name
dnl	$3 = includes
dnl	$4 = code fragment to compile/link
dnl	$5 = corresponding function-name
dnl	$6 = flag, nonnull if failure should not cause an error-exit
dnl
dnl Sets the variable "$cf_libdir" as a side-effect, so we can see if we had
dnl to use a -L option.
AC_DEFUN([CF_FIND_LIBRARY],
[
	eval 'cf_cv_have_lib_'"$1"'=no'
	cf_libdir=""
	AC_CHECK_FUNC($5,
		eval 'cf_cv_have_lib_'"$1"'=yes',[
		cf_save_LIBS="$LIBS"
		AC_MSG_CHECKING(for $5 in -l$1)
		LIBS="-l$1 $LIBS"
		AC_TRY_LINK([$3],[$4],
			[AC_MSG_RESULT(yes)
			 eval 'cf_cv_have_lib_'"$1"'=yes'
			],
			[AC_MSG_RESULT(no)
			CF_LIBRARY_PATH(cf_search,$2)
			for cf_libdir in $cf_search
			do
				AC_MSG_CHECKING(for -l$1 in $cf_libdir)
				LIBS="-L$cf_libdir -l$1 $cf_save_LIBS"
				AC_TRY_LINK([$3],[$4],
					[AC_MSG_RESULT(yes)
			 		 eval 'cf_cv_have_lib_'"$1"'=yes'
					 break],
					[AC_MSG_RESULT(no)
					 LIBS="$cf_save_LIBS"])
			done
			])
		])
eval 'cf_found_library="[$]cf_cv_have_lib_'"$1"\"
ifelse($6,,[
if test "$cf_found_library" = no ; then
	AC_MSG_ERROR(Cannot link $1 library)
fi
])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_FIND_LINKAGE version: 22 updated: 2020/12/31 20:19:42
dnl ---------------
dnl Find a library (specifically the linkage used in the code fragment),
dnl searching for it if it is not already in the library path.
dnl See also CF_ADD_SEARCHPATH.
dnl
dnl Parameters (4-on are optional):
dnl     $1 = headers for library entrypoint
dnl     $2 = code fragment for library entrypoint
dnl     $3 = the library name without the "-l" option or ".so" suffix.
dnl     $4 = action to perform if successful (default: update CPPFLAGS, etc)
dnl     $5 = action to perform if not successful
dnl     $6 = module name, if not the same as the library name
dnl     $7 = extra libraries
dnl
dnl Sets these variables:
dnl     $cf_cv_find_linkage_$3 - yes/no according to whether linkage is found
dnl     $cf_cv_header_path_$3 - include-directory if needed
dnl     $cf_cv_library_path_$3 - library-directory if needed
dnl     $cf_cv_library_file_$3 - library-file if needed, e.g., -l$3
AC_DEFUN([CF_FIND_LINKAGE],[

# If the linkage is not already in the $CPPFLAGS/$LDFLAGS configuration, these
# will be set on completion of the AC_TRY_LINK below.
cf_cv_header_path_$3=
cf_cv_library_path_$3=

CF_MSG_LOG([Starting [FIND_LINKAGE]($3,$6)])

cf_save_LIBS="$LIBS"

AC_TRY_LINK([$1],[$2],[
	cf_cv_find_linkage_$3=yes
	cf_cv_header_path_$3=/usr/include
	cf_cv_library_path_$3=/usr/lib
],[

LIBS="-l$3 $7 $cf_save_LIBS"

AC_TRY_LINK([$1],[$2],[
	cf_cv_find_linkage_$3=yes
	cf_cv_header_path_$3=/usr/include
	cf_cv_library_path_$3=/usr/lib
	cf_cv_library_file_$3="-l$3"
],[
	cf_cv_find_linkage_$3=no
	LIBS="$cf_save_LIBS"

	CF_VERBOSE(find linkage for $3 library)
	CF_MSG_LOG([Searching for headers in [FIND_LINKAGE]($3,$6)])

	cf_save_CPPFLAGS="$CPPFLAGS"
	cf_test_CPPFLAGS="$CPPFLAGS"

	CF_HEADER_PATH(cf_search,ifelse([$6],,[$3],[$6]))
	for cf_cv_header_path_$3 in $cf_search
	do
		if test -d "$cf_cv_header_path_$3" ; then
			CF_VERBOSE(... testing $cf_cv_header_path_$3)
			CPPFLAGS="$cf_save_CPPFLAGS"
			CF_APPEND_TEXT(CPPFLAGS,-I$cf_cv_header_path_$3)
			AC_TRY_COMPILE([$1],[$2],[
				CF_VERBOSE(... found $3 headers in $cf_cv_header_path_$3)
				cf_cv_find_linkage_$3=maybe
				cf_test_CPPFLAGS="$CPPFLAGS"
				break],[
				CPPFLAGS="$cf_save_CPPFLAGS"
				])
		fi
	done

	if test "$cf_cv_find_linkage_$3" = maybe ; then

		CF_MSG_LOG([Searching for $3 library in [FIND_LINKAGE]($3,$6)])

		cf_save_LIBS="$LIBS"
		cf_save_LDFLAGS="$LDFLAGS"

		ifelse([$6],,,[
		CPPFLAGS="$cf_test_CPPFLAGS"
		LIBS="-l$3 $7 $cf_save_LIBS"
		AC_TRY_LINK([$1],[$2],[
			CF_VERBOSE(... found $3 library in system)
			cf_cv_find_linkage_$3=yes])
			CPPFLAGS="$cf_save_CPPFLAGS"
			LIBS="$cf_save_LIBS"
			])

		if test "$cf_cv_find_linkage_$3" != yes ; then
			CF_LIBRARY_PATH(cf_search,$3)
			for cf_cv_library_path_$3 in $cf_search
			do
				if test -d "$cf_cv_library_path_$3" ; then
					CF_VERBOSE(... testing $cf_cv_library_path_$3)
					CPPFLAGS="$cf_test_CPPFLAGS"
					LIBS="-l$3 $7 $cf_save_LIBS"
					LDFLAGS="$cf_save_LDFLAGS -L$cf_cv_library_path_$3"
					AC_TRY_LINK([$1],[$2],[
					CF_VERBOSE(... found $3 library in $cf_cv_library_path_$3)
					cf_cv_find_linkage_$3=yes
					cf_cv_library_file_$3="-l$3"
					break],[
					CPPFLAGS="$cf_save_CPPFLAGS"
					LIBS="$cf_save_LIBS"
					LDFLAGS="$cf_save_LDFLAGS"
					])
				fi
			done
			CPPFLAGS="$cf_save_CPPFLAGS"
			LDFLAGS="$cf_save_LDFLAGS"
		fi

	else
		cf_cv_find_linkage_$3=no
	fi
	],$7)
])

LIBS="$cf_save_LIBS"

if test "$cf_cv_find_linkage_$3" = yes ; then
ifelse([$4],,[
	CF_ADD_INCDIR($cf_cv_header_path_$3)
	CF_ADD_LIBDIR($cf_cv_library_path_$3)
	CF_ADD_LIB($3)
],[$4])
else
ifelse([$5],,AC_MSG_WARN(Cannot find $3 library),[$5])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_FIX_WARNINGS version: 3 updated: 2020/12/31 18:40:20
dnl ---------------
dnl Warning flags do not belong in CFLAGS, CPPFLAGS, etc.  Any of gcc's
dnl "-Werror" flags can interfere with configure-checks.  Those go into
dnl EXTRA_CFLAGS.
dnl
dnl $1 = variable name to repair
define([CF_FIX_WARNINGS],[
if test "$GCC" = yes || test "$GXX" = yes
then
	case [$]$1 in
	(*-Werror=*)
		CF_VERBOSE(repairing $1: [$]$1)
		cf_temp_flags=
		for cf_temp_scan in [$]$1
		do
			case "x$cf_temp_scan" in
			(x-Werror=*)
				CF_APPEND_TEXT(EXTRA_CFLAGS,$cf_temp_scan)
				;;
			(*)
				CF_APPEND_TEXT(cf_temp_flags,$cf_temp_scan)
				;;
			esac
		done
		$1="$cf_temp_flags"
		CF_VERBOSE(... fixed [$]$1)
		CF_VERBOSE(... extra $EXTRA_CFLAGS)
		;;
	esac
fi
AC_SUBST(EXTRA_CFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_FUNC_CURSES_VERSION version: 8 updated: 2021/01/02 09:31:20
dnl ----------------------
dnl Solaris has a data item 'curses_version', which confuses AC_CHECK_FUNCS.
dnl It's a character string "SVR4", not documented.
AC_DEFUN([CF_FUNC_CURSES_VERSION],
[
AC_CACHE_CHECK(for function curses_version, cf_cv_func_curses_version,[
AC_TRY_RUN([
#include <${cf_cv_ncurses_header:-curses.h}>
int main(void)
{
	char temp[1024];
	sprintf(temp, "%s\\n", curses_version());
	${cf_cv_main_return:-return}(0);
}]
,[cf_cv_func_curses_version=yes]
,[cf_cv_func_curses_version=no]
,[cf_cv_func_curses_version=unknown])
rm -f core])
test "$cf_cv_func_curses_version" = yes && AC_DEFINE(HAVE_CURSES_VERSION,1,[Define to 1 if we have curses_version function])
])
dnl ---------------------------------------------------------------------------
dnl CF_FUNC_OPENPTY version: 6 updated: 2021/01/01 13:31:04
dnl ---------------
dnl Check for openpty() function, along with <pty.h> header.  It may need the
dnl "util" library as well.
AC_DEFUN([CF_FUNC_OPENPTY],
[
AC_CHECK_LIB(util,openpty,cf_cv_lib_util=yes,cf_cv_lib_util=no)
AC_CACHE_CHECK(for openpty header,cf_cv_func_openpty,[
	cf_save_LIBS="$LIBS"
	test "$cf_cv_lib_util" = yes && { CF_ADD_LIB(util) }
	for cf_header in pty.h libutil.h util.h
	do
	AC_TRY_LINK([
#include <$cf_header>
],[
	int x = openpty((int *)0, (int *)0, (char *)0,
				   (struct termios *)0, (struct winsize *)0);
],[
		cf_cv_func_openpty=$cf_header
		break
],[
		cf_cv_func_openpty=no
])
	done
	LIBS="$cf_save_LIBS"
])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_GCC_ATTRIBUTES version: 24 updated: 2021/03/20 12:00:25
dnl -----------------
dnl Test for availability of useful gcc __attribute__ directives to quiet
dnl compiler warnings.  Though useful, not all are supported -- and contrary
dnl to documentation, unrecognized directives cause older compilers to barf.
AC_DEFUN([CF_GCC_ATTRIBUTES],
[AC_REQUIRE([AC_PROG_FGREP])dnl
AC_REQUIRE([CF_C11_NORETURN])dnl

if test "$GCC" = yes || test "$GXX" = yes
then
cat > conftest.i <<EOF
#ifndef GCC_PRINTF
#define GCC_PRINTF 0
#endif
#ifndef GCC_SCANF
#define GCC_SCANF 0
#endif
#ifndef GCC_NORETURN
#define GCC_NORETURN /* nothing */
#endif
#ifndef GCC_UNUSED
#define GCC_UNUSED /* nothing */
#endif
EOF
if test "$GCC" = yes
then
	AC_CHECKING([for $CC __attribute__ directives])
cat > "conftest.$ac_ext" <<EOF
#line __oline__ "${as_me:-configure}"
#include "confdefs.h"
#include "conftest.h"
#include "conftest.i"
#if	GCC_PRINTF
#define GCC_PRINTFLIKE(fmt,var) __attribute__((format(printf,fmt,var)))
#else
#define GCC_PRINTFLIKE(fmt,var) /*nothing*/
#endif
#if	GCC_SCANF
#define GCC_SCANFLIKE(fmt,var)  __attribute__((format(scanf,fmt,var)))
#else
#define GCC_SCANFLIKE(fmt,var)  /*nothing*/
#endif
extern void wow(char *,...) GCC_SCANFLIKE(1,2);
extern GCC_NORETURN void oops(char *,...) GCC_PRINTFLIKE(1,2);
extern GCC_NORETURN void foo(void);
int main(int argc GCC_UNUSED, char *argv[[]] GCC_UNUSED) { (void)argc; (void)argv; return 0; }
EOF
	cf_printf_attribute=no
	cf_scanf_attribute=no
	for cf_attribute in scanf printf unused noreturn
	do
		CF_UPPER(cf_ATTRIBUTE,$cf_attribute)
		cf_directive="__attribute__(($cf_attribute))"
		echo "checking for $CC $cf_directive" 1>&AC_FD_CC

		case "$cf_attribute" in
		(printf)
			cf_printf_attribute=yes
			cat >conftest.h <<EOF
#define GCC_$cf_ATTRIBUTE 1
EOF
			;;
		(scanf)
			cf_scanf_attribute=yes
			cat >conftest.h <<EOF
#define GCC_$cf_ATTRIBUTE 1
EOF
			;;
		(*)
			cat >conftest.h <<EOF
#define GCC_$cf_ATTRIBUTE $cf_directive
EOF
			;;
		esac

		if AC_TRY_EVAL(ac_compile); then
			test -n "$verbose" && AC_MSG_RESULT(... $cf_attribute)
			cat conftest.h >>confdefs.h
			case "$cf_attribute" in
			(noreturn)
				AC_DEFINE_UNQUOTED(GCC_NORETURN,$cf_directive,[Define to noreturn-attribute for gcc])
				;;
			(printf)
				cf_value='/* nothing */'
				if test "$cf_printf_attribute" != no ; then
					cf_value='__attribute__((format(printf,fmt,var)))'
					AC_DEFINE(GCC_PRINTF,1,[Define to 1 if the compiler supports gcc-like printf attribute.])
				fi
				AC_DEFINE_UNQUOTED(GCC_PRINTFLIKE(fmt,var),$cf_value,[Define to printf-attribute for gcc])
				;;
			(scanf)
				cf_value='/* nothing */'
				if test "$cf_scanf_attribute" != no ; then
					cf_value='__attribute__((format(scanf,fmt,var)))'
					AC_DEFINE(GCC_SCANF,1,[Define to 1 if the compiler supports gcc-like scanf attribute.])
				fi
				AC_DEFINE_UNQUOTED(GCC_SCANFLIKE(fmt,var),$cf_value,[Define to sscanf-attribute for gcc])
				;;
			(unused)
				AC_DEFINE_UNQUOTED(GCC_UNUSED,$cf_directive,[Define to unused-attribute for gcc])
				;;
			esac
		fi
	done
else
	${FGREP-fgrep} define conftest.i >>confdefs.h
fi
rm -rf ./conftest*
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_GCC_VERSION version: 8 updated: 2019/09/07 13:38:36
dnl --------------
dnl Find version of gcc, and (because icc/clang pretend to be gcc without being
dnl compatible), attempt to determine if icc/clang is actually used.
AC_DEFUN([CF_GCC_VERSION],[
AC_REQUIRE([AC_PROG_CC])
GCC_VERSION=none
if test "$GCC" = yes ; then
	AC_MSG_CHECKING(version of $CC)
	GCC_VERSION="`${CC} --version 2>/dev/null | sed -e '2,$d' -e 's/^.*(GCC[[^)]]*) //' -e 's/^.*(Debian[[^)]]*) //' -e 's/^[[^0-9.]]*//' -e 's/[[^0-9.]].*//'`"
	test -z "$GCC_VERSION" && GCC_VERSION=unknown
	AC_MSG_RESULT($GCC_VERSION)
fi
CF_INTEL_COMPILER(GCC,INTEL_COMPILER,CFLAGS)
CF_CLANG_COMPILER(GCC,CLANG_COMPILER,CFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_GCC_WARNINGS version: 41 updated: 2021/01/01 16:53:59
dnl ---------------
dnl Check if the compiler supports useful warning options.  There's a few that
dnl we don't use, simply because they're too noisy:
dnl
dnl	-Wconversion (useful in older versions of gcc, but not in gcc 2.7.x)
dnl	-Winline (usually not worthwhile)
dnl	-Wredundant-decls (system headers make this too noisy)
dnl	-Wtraditional (combines too many unrelated messages, only a few useful)
dnl	-Wwrite-strings (too noisy, but should review occasionally).  This
dnl		is enabled for ncurses using "--enable-const".
dnl	-pedantic
dnl
dnl Parameter:
dnl	$1 is an optional list of gcc warning flags that a particular
dnl		application might want to use, e.g., "no-unused" for
dnl		-Wno-unused
dnl Special:
dnl	If $with_ext_const is "yes", add a check for -Wwrite-strings
dnl
AC_DEFUN([CF_GCC_WARNINGS],
[
AC_REQUIRE([CF_GCC_VERSION])
if test "x$have_x" = xyes; then CF_CONST_X_STRING fi
cat > "conftest.$ac_ext" <<EOF
#line __oline__ "${as_me:-configure}"
int main(int argc, char *argv[[]]) { return (argv[[argc-1]] == 0) ; }
EOF
if test "$INTEL_COMPILER" = yes
then
# The "-wdXXX" options suppress warnings:
# remark #1419: external declaration in primary source file
# remark #1683: explicit conversion of a 64-bit integral type to a smaller integral type (potential portability problem)
# remark #1684: conversion from pointer to same-sized integral type (potential portability problem)
# remark #193: zero used for undefined preprocessing identifier
# remark #593: variable "curs_sb_left_arrow" was set but never used
# remark #810: conversion from "int" to "Dimension={unsigned short}" may lose significant bits
# remark #869: parameter "tw" was never referenced
# remark #981: operands are evaluated in unspecified order
# warning #279: controlling expression is constant

	AC_CHECKING([for $CC warning options])
	cf_save_CFLAGS="$CFLAGS"
	EXTRA_CFLAGS="$EXTRA_CFLAGS -Wall"
	for cf_opt in \
		wd1419 \
		wd1683 \
		wd1684 \
		wd193 \
		wd593 \
		wd279 \
		wd810 \
		wd869 \
		wd981
	do
		CFLAGS="$cf_save_CFLAGS $EXTRA_CFLAGS -$cf_opt"
		if AC_TRY_EVAL(ac_compile); then
			test -n "$verbose" && AC_MSG_RESULT(... -$cf_opt)
			EXTRA_CFLAGS="$EXTRA_CFLAGS -$cf_opt"
		fi
	done
	CFLAGS="$cf_save_CFLAGS"
elif test "$GCC" = yes && test "$GCC_VERSION" != "unknown"
then
	AC_CHECKING([for $CC warning options])
	cf_save_CFLAGS="$CFLAGS"
	cf_warn_CONST=""
	test "$with_ext_const" = yes && cf_warn_CONST="Wwrite-strings"
	cf_gcc_warnings="Wignored-qualifiers Wlogical-op Wvarargs"
	test "x$CLANG_COMPILER" = xyes && cf_gcc_warnings=
	for cf_opt in W Wall \
		Wbad-function-cast \
		Wcast-align \
		Wcast-qual \
		Wdeclaration-after-statement \
		Wextra \
		Winline \
		Wmissing-declarations \
		Wmissing-prototypes \
		Wnested-externs \
		Wpointer-arith \
		Wshadow \
		Wstrict-prototypes \
		Wundef Wno-inline $cf_gcc_warnings $cf_warn_CONST $1
	do
		CFLAGS="$cf_save_CFLAGS $EXTRA_CFLAGS -$cf_opt"
		if AC_TRY_EVAL(ac_compile); then
			test -n "$verbose" && AC_MSG_RESULT(... -$cf_opt)
			case "$cf_opt" in
			(Winline)
				case "$GCC_VERSION" in
				([[34]].*)
					CF_VERBOSE(feature is broken in gcc $GCC_VERSION)
					continue;;
				esac
				;;
			(Wpointer-arith)
				case "$GCC_VERSION" in
				([[12]].*)
					CF_VERBOSE(feature is broken in gcc $GCC_VERSION)
					continue;;
				esac
				;;
			esac
			EXTRA_CFLAGS="$EXTRA_CFLAGS -$cf_opt"
		fi
	done
	CFLAGS="$cf_save_CFLAGS"
fi
rm -rf ./conftest*

AC_SUBST(EXTRA_CFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_GETOPT_HEADER version: 8 updated: 2021/06/19 19:16:16
dnl ----------------
dnl Check for getopt's variables which are commonly defined in stdlib.h,
dnl unistd.h or (nonstandard) in getopt.h
AC_DEFUN([CF_GETOPT_HEADER],
[
AC_HAVE_HEADERS(unistd.h getopt.h)
AC_CACHE_CHECK(for header declaring getopt variables,cf_cv_getopt_header,[
cf_cv_getopt_header=none
for cf_header in stdio.h stdlib.h unistd.h getopt.h
do
AC_TRY_COMPILE([
#include <$cf_header>],
[int x = optind; char *y = optarg; (void)x; (void)y],
[cf_cv_getopt_header=$cf_header
 break])
done
])
if test "$cf_cv_getopt_header" != none ; then
	AC_DEFINE(HAVE_GETOPT_HEADER,1,[Define to 1 if getopt variables are declared in header])
fi
if test "$cf_cv_getopt_header" = getopt.h ; then
	AC_DEFINE(NEED_GETOPT_H,1,[Define to 1 if we must include getopt.h])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_GNU_SOURCE version: 10 updated: 2018/12/10 20:09:41
dnl -------------
dnl Check if we must define _GNU_SOURCE to get a reasonable value for
dnl _XOPEN_SOURCE, upon which many POSIX definitions depend.  This is a defect
dnl (or misfeature) of glibc2, which breaks portability of many applications,
dnl since it is interwoven with GNU extensions.
dnl
dnl Well, yes we could work around it...
dnl
dnl Parameters:
dnl	$1 is the nominal value for _XOPEN_SOURCE
AC_DEFUN([CF_GNU_SOURCE],
[
cf_gnu_xopen_source=ifelse($1,,500,$1)

AC_CACHE_CHECK(if this is the GNU C library,cf_cv_gnu_library,[
AC_TRY_COMPILE([#include <sys/types.h>],[
	#if __GLIBC__ > 0 && __GLIBC_MINOR__ >= 0
		return 0;
	#elif __NEWLIB__ > 0 && __NEWLIB_MINOR__ >= 0
		return 0;
	#else
	#	error not GNU C library
	#endif],
	[cf_cv_gnu_library=yes],
	[cf_cv_gnu_library=no])
])

if test x$cf_cv_gnu_library = xyes; then

	# With glibc 2.19 (13 years after this check was begun), _DEFAULT_SOURCE
	# was changed to help a little.  newlib incorporated the change about 4
	# years later.
	AC_CACHE_CHECK(if _DEFAULT_SOURCE can be used as a basis,cf_cv_gnu_library_219,[
		cf_save="$CPPFLAGS"
		CF_APPEND_TEXT(CPPFLAGS,-D_DEFAULT_SOURCE)
		AC_TRY_COMPILE([#include <sys/types.h>],[
			#if (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 19) || (__GLIBC__ > 2)
				return 0;
			#elif (__NEWLIB__ == 2 && __NEWLIB_MINOR__ >= 4) || (__GLIBC__ > 3)
				return 0;
			#else
			#	error GNU C library __GLIBC__.__GLIBC_MINOR__ is too old
			#endif],
			[cf_cv_gnu_library_219=yes],
			[cf_cv_gnu_library_219=no])
		CPPFLAGS="$cf_save"
	])

	if test "x$cf_cv_gnu_library_219" = xyes; then
		cf_save="$CPPFLAGS"
		AC_CACHE_CHECK(if _XOPEN_SOURCE=$cf_gnu_xopen_source works with _DEFAULT_SOURCE,cf_cv_gnu_dftsrc_219,[
			CF_ADD_CFLAGS(-D_DEFAULT_SOURCE -D_XOPEN_SOURCE=$cf_gnu_xopen_source)
			AC_TRY_COMPILE([
				#include <limits.h>
				#include <sys/types.h>
				],[
				#if (_XOPEN_SOURCE >= $cf_gnu_xopen_source) && (MB_LEN_MAX > 1)
					return 0;
				#else
				#	error GNU C library is too old
				#endif],
				[cf_cv_gnu_dftsrc_219=yes],
				[cf_cv_gnu_dftsrc_219=no])
			])
		test "x$cf_cv_gnu_dftsrc_219" = "xyes" || CPPFLAGS="$cf_save"
	else
		cf_cv_gnu_dftsrc_219=maybe
	fi

	if test "x$cf_cv_gnu_dftsrc_219" != xyes; then

		AC_CACHE_CHECK(if we must define _GNU_SOURCE,cf_cv_gnu_source,[
		AC_TRY_COMPILE([#include <sys/types.h>],[
			#ifndef _XOPEN_SOURCE
			#error	expected _XOPEN_SOURCE to be defined
			#endif],
			[cf_cv_gnu_source=no],
			[cf_save="$CPPFLAGS"
			 CF_ADD_CFLAGS(-D_GNU_SOURCE)
			 AC_TRY_COMPILE([#include <sys/types.h>],[
				#ifdef _XOPEN_SOURCE
				#error	expected _XOPEN_SOURCE to be undefined
				#endif],
				[cf_cv_gnu_source=no],
				[cf_cv_gnu_source=yes])
			CPPFLAGS="$cf_save"
			])
		])

		if test "$cf_cv_gnu_source" = yes
		then
		AC_CACHE_CHECK(if we should also define _DEFAULT_SOURCE,cf_cv_default_source,[
			CF_APPEND_TEXT(CPPFLAGS,-D_GNU_SOURCE)
			AC_TRY_COMPILE([#include <sys/types.h>],[
				#ifdef _DEFAULT_SOURCE
				#error	expected _DEFAULT_SOURCE to be undefined
				#endif],
				[cf_cv_default_source=no],
				[cf_cv_default_source=yes])
			])
			if test "$cf_cv_default_source" = yes
			then
				CF_APPEND_TEXT(CPPFLAGS,-D_DEFAULT_SOURCE)
			fi
		fi
	fi

fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_HEADER_PATH version: 15 updated: 2021/01/01 13:31:04
dnl --------------
dnl Construct a search-list of directories for a nonstandard header-file
dnl
dnl Parameters
dnl	$1 = the variable to return as result
dnl	$2 = the package name
AC_DEFUN([CF_HEADER_PATH],
[
$1=

# collect the current set of include-directories from compiler flags
cf_header_path_list=""
if test -n "${CFLAGS}${CPPFLAGS}" ; then
	for cf_header_path in $CPPFLAGS $CFLAGS
	do
		case "$cf_header_path" in
		(-I*)
			cf_header_path=`echo ".$cf_header_path" |sed -e 's/^...//' -e 's,/include$,,'`
			CF_ADD_SUBDIR_PATH($1,$2,include,$cf_header_path,NONE)
			cf_header_path_list="$cf_header_path_list [$]$1"
			;;
		esac
	done
fi

# add the variations for the package we are looking for
CF_SUBDIR_PATH($1,$2,include)

test "$includedir" != NONE && \
test "$includedir" != "/usr/include" && \
test -d "$includedir" && {
	test -d "$includedir" &&    $1="[$]$1 $includedir"
	test -d "$includedir/$2" && $1="[$]$1 $includedir/$2"
}

test "$oldincludedir" != NONE && \
test "$oldincludedir" != "/usr/include" && \
test -d "$oldincludedir" && {
	test -d "$oldincludedir"    && $1="[$]$1 $oldincludedir"
	test -d "$oldincludedir/$2" && $1="[$]$1 $oldincludedir/$2"
}

$1="[$]$1 $cf_header_path_list"
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_HELP_MESSAGE version: 4 updated: 2019/12/31 08:53:54
dnl ---------------
dnl Insert text into the help-message, for readability, from AC_ARG_WITH.
AC_DEFUN([CF_HELP_MESSAGE],
[CF_ACVERSION_CHECK(2.53,[],[
AC_DIVERT_HELP($1)])dnl
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_INHERIT_SCRIPT version: 2 updated: 2003/03/01 23:50:42
dnl -----------------
dnl If we do not have a given script, look for it in the parent directory.
AC_DEFUN([CF_INHERIT_SCRIPT],
[
test -f $1 || ( test -f ../$1 && cp ../$1 ./ )
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_INSTALL_OPTS version: 2 updated: 2018/08/18 12:19:21
dnl ---------------
dnl prompt for/fill-in useful install-program options
AC_DEFUN([CF_INSTALL_OPTS],
[
CF_INSTALL_OPT_S
CF_INSTALL_OPT_P
CF_INSTALL_OPT_O
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_INSTALL_OPT_O version: 3 updated: 2020/12/31 20:19:42
dnl ----------------
dnl Almost all "install" programs default to the current user's ownership.
dnl Almost - MINIX is an exception.
AC_DEFUN([CF_INSTALL_OPT_O],
[
AC_MSG_CHECKING(if install needs to be told about ownership)
case `$ac_config_guess` in
(*minix)
	with_install_o=yes
	;;
(*)
	with_install_o=no
	;;
esac

AC_MSG_RESULT($with_install_o)
if test "x$with_install_o" = xyes
then
	INSTALL_OPT_O="`id root|sed -e 's/uid=[[0-9]]*(/ -o /' -e 's/gid=[[0-9]]*(/ -g /' -e 's/ [[^=[:space:]]][[^=[:space:]]]*=.*/ /' -e 's/)//g'`"
else
	INSTALL_OPT_O=
fi

AC_SUBST(INSTALL_OPT_O)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_INSTALL_OPT_P version: 3 updated: 2021/01/01 13:31:04
dnl ----------------
dnl Some install-programs accept a "-p" option to preserve file modification
dnl timestamps.  That can be useful as an install option, as well as a way to
dnl avoid the need for ranlib after copying a static archive.
AC_DEFUN([CF_INSTALL_OPT_P],
[
: "${INSTALL:=install}"
AC_CACHE_CHECK(if install accepts -p option, cf_cv_install_p,[
	rm -rf ./conftest*
	date >conftest.in
	mkdir conftest.out
	sleep 3
	if $INSTALL -p conftest.in conftest.out 2>/dev/null
	then
		if test -f conftest.out/conftest.in
		then
			test conftest.in -nt conftest.out/conftest.in 2>conftest.err && \
			test conftest.out/conftest.in -nt conftest.in 2>conftest.err
			if test -s conftest.err
			then
				cf_cv_install_p=no
			else
				cf_cv_install_p=yes
			fi
		else
			cf_cv_install_p=no
		fi
	else
		cf_cv_install_p=no
	fi
	rm -rf ./conftest*
])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_INSTALL_OPT_S version: 3 updated: 2021/01/05 19:23:48
dnl ----------------
dnl By default, we should strip executables which are installed, but leave the
dnl ability to suppress that for unit-testing.
AC_DEFUN([CF_INSTALL_OPT_S],
[
AC_MSG_CHECKING(if you want to install stripped executables)
CF_ARG_DISABLE(stripping,
	[  --disable-stripping     do not strip (debug info) installed executables],
	[enable_stripping=no],
	[enable_stripping=yes])
AC_MSG_RESULT($enable_stripping)

if test "$enable_stripping" = yes
then
	INSTALL_OPT_S="-s"
else
	INSTALL_OPT_S=
fi
AC_SUBST(INSTALL_OPT_S)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_INTEL_COMPILER version: 8 updated: 2021/01/01 16:53:59
dnl -----------------
dnl Check if the given compiler is really the Intel compiler for Linux.  It
dnl tries to imitate gcc, but does not return an error when it finds a mismatch
dnl between prototypes, e.g., as exercised by CF_MISSING_CHECK.
dnl
dnl This macro should be run "soon" after AC_PROG_CC or AC_PROG_CPLUSPLUS, to
dnl ensure that it is not mistaken for gcc/g++.  It is normally invoked from
dnl the wrappers for gcc and g++ warnings.
dnl
dnl $1 = GCC (default) or GXX
dnl $2 = INTEL_COMPILER (default) or INTEL_CPLUSPLUS
dnl $3 = CFLAGS (default) or CXXFLAGS
AC_DEFUN([CF_INTEL_COMPILER],[
AC_REQUIRE([AC_CANONICAL_HOST])
ifelse([$2],,INTEL_COMPILER,[$2])=no

if test "$ifelse([$1],,[$1],GCC)" = yes ; then
	case "$host_os" in
	(linux*|gnu*)
		AC_MSG_CHECKING(if this is really Intel ifelse([$1],GXX,C++,C) compiler)
		cf_save_CFLAGS="$ifelse([$3],,CFLAGS,[$3])"
		ifelse([$3],,CFLAGS,[$3])="$ifelse([$3],,CFLAGS,[$3]) -no-gcc"
		AC_TRY_COMPILE([],[
#ifdef __INTEL_COMPILER
#else
make an error
#endif
],[ifelse([$2],,INTEL_COMPILER,[$2])=yes
cf_save_CFLAGS="$cf_save_CFLAGS -we147"
],[])
		ifelse([$3],,CFLAGS,[$3])="$cf_save_CFLAGS"
		AC_MSG_RESULT($ifelse([$2],,INTEL_COMPILER,[$2]))
		;;
	esac
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_LD_RPATH_OPT version: 9 updated: 2021/01/01 13:31:04
dnl ---------------
dnl For the given system and compiler, find the compiler flags to pass to the
dnl loader to use the "rpath" feature.
AC_DEFUN([CF_LD_RPATH_OPT],
[
AC_REQUIRE([CF_CHECK_CACHE])

LD_RPATH_OPT=
if test "x$cf_cv_enable_rpath" != xno
then
	AC_MSG_CHECKING(for an rpath option)
	case "$cf_cv_system_name" in
	(irix*)
		if test "$GCC" = yes; then
			LD_RPATH_OPT="-Wl,-rpath,"
		else
			LD_RPATH_OPT="-rpath "
		fi
		;;
	(linux*|gnu*|k*bsd*-gnu|freebsd*)
		LD_RPATH_OPT="-Wl,-rpath,"
		;;
	(openbsd[[2-9]].*|mirbsd*)
		LD_RPATH_OPT="-Wl,-rpath,"
		;;
	(dragonfly*)
		LD_RPATH_OPT="-rpath "
		;;
	(netbsd*)
		LD_RPATH_OPT="-Wl,-rpath,"
		;;
	(osf*|mls+*)
		LD_RPATH_OPT="-rpath "
		;;
	(solaris2*)
		LD_RPATH_OPT="-R"
		;;
	(*)
		;;
	esac
	AC_MSG_RESULT($LD_RPATH_OPT)

	case "x$LD_RPATH_OPT" in
	(x-R*)
		AC_MSG_CHECKING(if we need a space after rpath option)
		cf_save_LIBS="$LIBS"
		CF_ADD_LIBS(${LD_RPATH_OPT}$libdir)
		AC_TRY_LINK(, , cf_rpath_space=no, cf_rpath_space=yes)
		LIBS="$cf_save_LIBS"
		AC_MSG_RESULT($cf_rpath_space)
		test "$cf_rpath_space" = yes && LD_RPATH_OPT="$LD_RPATH_OPT "
		;;
	esac
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_LIBRARY_PATH version: 11 updated: 2021/01/01 13:31:04
dnl ---------------
dnl Construct a search-list of directories for a nonstandard library-file
dnl
dnl Parameters
dnl	$1 = the variable to return as result
dnl	$2 = the package name
AC_DEFUN([CF_LIBRARY_PATH],
[
$1=
cf_library_path_list=""
if test -n "${LDFLAGS}${LIBS}" ; then
	for cf_library_path in $LDFLAGS $LIBS
	do
		case "$cf_library_path" in
		(-L*)
			cf_library_path=`echo ".$cf_library_path" |sed -e 's/^...//' -e 's,/lib$,,'`
			CF_ADD_SUBDIR_PATH($1,$2,lib,$cf_library_path,NONE)
			cf_library_path_list="$cf_library_path_list [$]$1"
			;;
		esac
	done
fi

CF_SUBDIR_PATH($1,$2,lib)

$1="$cf_library_path_list [$]$1"
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_MAKE_PHONY version: 3 updated: 2021/01/08 16:08:21
dnl -------------
dnl Check if the make-program handles a ".PHONY" target, e.g,. a target which
dnl acts as a placeholder.
dnl
dnl The ".PHONY" feature was proposed in 2011 here
dnl     https://www.austingroupbugs.net/view.php?id=523
dnl and is scheduled for release in P1003.1 Issue 8 (late 2022).
dnl
dnl This is not supported by SVr4 make (or SunOS 4, 4.3SD, etc), but works with
dnl a few others (i.e., GNU make and the non-POSIX "BSD" make):
dnl
dnl + This is a GNU make feature (since April 1988, but in turn from binutils,
dnl   date unspecified).
dnl
dnl + It was adopted in NetBSD make in June 1995.
dnl
dnl + The other BSD make programs are derived from the NetBSD make (and for
dnl   that reason are not actually different "implementations").
dnl
dnl + Some features of NetBSD make were actually adapted from pmake, which
dnl   began as a modified GNU make starting in 1993.
dnl
dnl + Version 3.8 of the dmake program in January 1992 also implemented this
dnl   GNU make extension, but is less well known than the BSD make.
AC_DEFUN([CF_MAKE_PHONY],[
AC_CACHE_CHECK(for \".PHONY\" make-support, cf_cv_make_PHONY,[
	rm -rf conftest*
	(
		mkdir conftest || exit 1
		cd conftest
		cat >makefile <<'CF_EOF'
.PHONY: always
DATA=0
always:	always.out
	@echo "** making [$]@ [$](DATA)"
once: once.out
	@echo "** making [$]@ [$](DATA)"
always.out:
	@echo "** making [$]@ [$](DATA)"
	echo [$](DATA) > [$]@
once.out:
	@echo "** making [$]@ [$](DATA)"
	echo [$](DATA) > [$]@
CF_EOF
		for cf_data in 1 2 3
		do
			${MAKE:-make} always DATA=$cf_data
			${MAKE:-make} once   DATA=$cf_data
			${MAKE:-make} -t always once
			if test -f always ; then
				echo "no (case 1)" > ../conftest.tmp
			elif test ! -f always.out ; then
				echo "no (case 2)" > ../conftest.tmp
			elif test ! -f once.out ; then
				echo "no (case 3)" > ../conftest.tmp
			elif ! cmp -s always.out once.out ; then
				echo "no (case 4)" > ../conftest.tmp
				diff always.out once.out
			else
				cf_check="`cat always.out`"
				if test "x$cf_check" != "x$cf_data" ; then
					echo "no (case 5)" > ../conftest.tmp
				else
					echo yes > ../conftest.tmp
					rm -f ./*.out
					continue
				fi
			fi
			break
		done
	) >&AC_FD_CC 2>&1
	cf_cv_make_PHONY="`cat conftest.tmp`"
	rm -rf conftest*
])
MAKE_NO_PHONY="#"
MAKE_PHONY="#"
test "x$cf_cv_make_PHONY" = xyes && MAKE_PHONY=
test "x$cf_cv_make_PHONY" != xyes && MAKE_NO_PHONY=
AC_SUBST(MAKE_NO_PHONY)
AC_SUBST(MAKE_PHONY)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_MAKE_TAGS version: 6 updated: 2010/10/23 15:52:32
dnl ------------
dnl Generate tags/TAGS targets for makefiles.  Do not generate TAGS if we have
dnl a monocase filesystem.
AC_DEFUN([CF_MAKE_TAGS],[
AC_REQUIRE([CF_MIXEDCASE_FILENAMES])

AC_CHECK_PROGS(CTAGS, exctags ctags)
AC_CHECK_PROGS(ETAGS, exetags etags)

AC_CHECK_PROG(MAKE_LOWER_TAGS, ${CTAGS:-ctags}, yes, no)

if test "$cf_cv_mixedcase" = yes ; then
	AC_CHECK_PROG(MAKE_UPPER_TAGS, ${ETAGS:-etags}, yes, no)
else
	MAKE_UPPER_TAGS=no
fi

if test "$MAKE_UPPER_TAGS" = yes ; then
	MAKE_UPPER_TAGS=
else
	MAKE_UPPER_TAGS="#"
fi

if test "$MAKE_LOWER_TAGS" = yes ; then
	MAKE_LOWER_TAGS=
else
	MAKE_LOWER_TAGS="#"
fi

AC_SUBST(CTAGS)
AC_SUBST(ETAGS)

AC_SUBST(MAKE_UPPER_TAGS)
AC_SUBST(MAKE_LOWER_TAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_MATH_LIB version: 10 updated: 2020/12/31 18:40:20
dnl -----------
dnl Checks for libraries.  At least one UNIX system, Apple Macintosh
dnl Rhapsody 5.5, does not have -lm.  We cannot use the simpler
dnl AC_CHECK_LIB(m,sin), because that fails for C++.
AC_DEFUN([CF_MATH_LIB],
[
AC_CACHE_CHECK(if -lm needed for math functions,
	cf_cv_need_libm,[
	AC_TRY_LINK([
	#include <stdio.h>
	#include <stdlib.h>
	#include <math.h>
	],
	[double x = rand(); printf("result = %g\\n", ]ifelse([$2],,sin(x),$2)[)],
	[cf_cv_need_libm=no],
	[cf_cv_need_libm=yes])])
if test "$cf_cv_need_libm" = yes
then
ifelse($1,,[
	CF_ADD_LIB(m)
],[$1=-lm])
fi
])
dnl ---------------------------------------------------------------------------
dnl CF_MIXEDCASE_FILENAMES version: 9 updated: 2021/01/01 16:53:59
dnl ----------------------
dnl Check if the file-system supports mixed-case filenames.  If we're able to
dnl create a lowercase name and see it as uppercase, it doesn't support that.
AC_DEFUN([CF_MIXEDCASE_FILENAMES],
[
AC_CACHE_CHECK(if filesystem supports mixed-case filenames,cf_cv_mixedcase,[
if test "$cross_compiling" = yes ; then
	case "$target_alias" in
	(*-os2-emx*|*-msdosdjgpp*|*-cygwin*|*-msys*|*-mingw*|*-uwin*|darwin*)
		cf_cv_mixedcase=no
		;;
	(*)
		cf_cv_mixedcase=yes
		;;
	esac
else
	rm -f conftest CONFTEST
	echo test >conftest
	if test -f CONFTEST ; then
		cf_cv_mixedcase=no
	else
		cf_cv_mixedcase=yes
	fi
	rm -f conftest CONFTEST
fi
])
test "$cf_cv_mixedcase" = yes && AC_DEFINE(MIXEDCASE_FILENAMES,1,[Define to 1 if filesystem supports mixed-case filenames.])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_MSG_LOG version: 5 updated: 2010/10/23 15:52:32
dnl ----------
dnl Write a debug message to config.log, along with the line number in the
dnl configure script.
AC_DEFUN([CF_MSG_LOG],[
echo "${as_me:-configure}:__oline__: testing $* ..." 1>&AC_FD_CC
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_CC_CHECK version: 5 updated: 2020/12/31 20:19:42
dnl -------------------
dnl Check if we can compile with ncurses' header file
dnl $1 is the cache variable to set
dnl $2 is the header-file to include
dnl $3 is the root name (ncurses or ncursesw)
AC_DEFUN([CF_NCURSES_CC_CHECK],[
	AC_TRY_COMPILE([
]ifelse($3,ncursesw,[
#define _XOPEN_SOURCE_EXTENDED
#undef  HAVE_LIBUTF8_H	/* in case we used CF_UTF8_LIB */
#define HAVE_LIBUTF8_H	/* to force ncurses' header file to use cchar_t */
])[
#include <$2>],[
#ifdef NCURSES_VERSION
]ifelse($3,ncursesw,[
#ifndef WACS_BSSB
	make an error
#endif
])[
printf("%s\\n", NCURSES_VERSION);
#else
#ifdef __NCURSES_H
printf("old\\n");
#else
	make an error
#endif
#endif
	]
	,[$1=$2]
	,[$1=no])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_CONFIG version: 28 updated: 2021/08/28 15:20:37
dnl -----------------
dnl Tie together the configure-script macros for ncurses, preferring these in
dnl order:
dnl a) ".pc" files for pkg-config, using $NCURSES_CONFIG_PKG
dnl b) the "-config" script from ncurses, using $NCURSES_CONFIG
dnl c) just plain libraries
dnl
dnl $1 is the root library name (default: "ncurses")
AC_DEFUN([CF_NCURSES_CONFIG],[
AC_REQUIRE([CF_PKG_CONFIG])
cf_ncuconfig_root=ifelse($1,,ncurses,$1)
cf_have_ncuconfig=no

if test "x${PKG_CONFIG:=none}" != xnone; then
	AC_MSG_CHECKING(pkg-config for $cf_ncuconfig_root)
	if "$PKG_CONFIG" --exists $cf_ncuconfig_root ; then
		AC_MSG_RESULT(yes)

		AC_MSG_CHECKING(if the $cf_ncuconfig_root package files work)
		cf_have_ncuconfig=unknown

		cf_save_CFLAGS="$CFLAGS"
		cf_save_CPPFLAGS="$CPPFLAGS"
		cf_save_LIBS="$LIBS"

		cf_pkg_cflags="`$PKG_CONFIG --cflags $cf_ncuconfig_root`"
		cf_pkg_libs="`$PKG_CONFIG --libs $cf_ncuconfig_root`"

		# while -W for passing linker flags is prevalent, it is not "standard".
		# At least one wrapper for c89/c99 (in Apple's xcode) has its own
		# incompatible _and_ non-standard -W option which gives an error.  Work
		# around that pitfall.
		case "x${CC}@@${cf_pkg_libs}@${cf_pkg_cflags}" in
		(x*c[[89]]9@@*-W*)
			CF_ADD_CFLAGS($cf_pkg_cflags)
			CF_ADD_LIBS($cf_pkg_libs)

			AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
				[initscr(); mousemask(0,0); tigetstr((char *)0);],
				[AC_TRY_RUN([#include <${cf_cv_ncurses_header:-curses.h}>
					int main(void)
					{ const char *xx = curses_version(); return (xx == 0); }],
					[cf_test_ncuconfig=yes],
					[cf_test_ncuconfig=no],
					[cf_test_ncuconfig=maybe])],
				[cf_test_ncuconfig=no])

			CFLAGS="$cf_save_CFLAGS"
			CPPFLAGS="$cf_save_CPPFLAGS"
			LIBS="$cf_save_LIBS"

			if test "x$cf_test_ncuconfig" != xyes; then
				cf_temp=`echo "x$cf_pkg_cflags" | sed -e s/^x// -e 's/-W[[^ 	]]*//g'`
				cf_pkg_cflags="$cf_temp"
				cf_temp=`echo "x$cf_pkg_libs" | sed -e s/^x// -e 's/-W[[^ 	]]*//g'`
				cf_pkg_libs="$cf_temp"
			fi
			;;
		esac

		CF_APPEND_CFLAGS($cf_pkg_cflags)
		CF_ADD_LIBS($cf_pkg_libs)

		AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
			[initscr(); mousemask(0,0); tigetstr((char *)0);],
			[AC_TRY_RUN([#include <${cf_cv_ncurses_header:-curses.h}>
				int main(void)
				{ const char *xx = curses_version(); return (xx == 0); }],
				[cf_have_ncuconfig=yes],
				[cf_have_ncuconfig=no],
				[cf_have_ncuconfig=maybe])],
			[cf_have_ncuconfig=no])
		AC_MSG_RESULT($cf_have_ncuconfig)
		test "$cf_have_ncuconfig" = maybe && cf_have_ncuconfig=yes
		if test "$cf_have_ncuconfig" != "yes"
		then
			CPPFLAGS="$cf_save_CPPFLAGS"
			LIBS="$cf_save_LIBS"
			NCURSES_CONFIG_PKG=none
		else
			AC_DEFINE(NCURSES,1,[Define to 1 if we are using ncurses headers/libraries])
			NCURSES_CONFIG_PKG=$cf_ncuconfig_root
			CF_TERM_HEADER
		fi

	else
		AC_MSG_RESULT(no)
		NCURSES_CONFIG_PKG=none
	fi
else
	NCURSES_CONFIG_PKG=none
fi

if test "x$cf_have_ncuconfig" = "xno"; then
	cf_ncurses_config="${cf_ncuconfig_root}${NCURSES_CONFIG_SUFFIX}-config"; echo "Looking for ${cf_ncurses_config}"

	CF_ACVERSION_CHECK(2.52,
		[AC_CHECK_TOOLS(NCURSES_CONFIG, ${cf_ncurses_config} ${cf_ncuconfig_root}6-config ${cf_ncuconfig_root}6-config ${cf_ncuconfig_root}5-config, none)],
		[AC_PATH_PROGS(NCURSES_CONFIG,  ${cf_ncurses_config} ${cf_ncuconfig_root}6-config ${cf_ncuconfig_root}6-config ${cf_ncuconfig_root}5-config, none)])

	if test "$NCURSES_CONFIG" != none ; then

		CF_APPEND_CFLAGS(`$NCURSES_CONFIG --cflags`)
		CF_ADD_LIBS(`$NCURSES_CONFIG --libs`)

		# even with config script, some packages use no-override for curses.h
		CF_CURSES_HEADER(ifelse($1,,ncurses,$1))

		dnl like CF_NCURSES_CPPFLAGS
		AC_DEFINE(NCURSES,1,[Define to 1 if we are using ncurses headers/libraries])

		dnl like CF_NCURSES_LIBS
		CF_UPPER(cf_nculib_ROOT,HAVE_LIB$cf_ncuconfig_root)
		AC_DEFINE_UNQUOTED($cf_nculib_ROOT)

		dnl like CF_NCURSES_VERSION
		cf_cv_ncurses_version="`$NCURSES_CONFIG --version`"

	else

		CF_NCURSES_CPPFLAGS(ifelse($1,,ncurses,$1))
		CF_NCURSES_LIBS(ifelse($1,,ncurses,$1))

	fi
else
	NCURSES_CONFIG=none
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_CPPFLAGS version: 22 updated: 2021/01/02 09:31:20
dnl -------------------
dnl Look for the SVr4 curses clone 'ncurses' in the standard places, adjusting
dnl the CPPFLAGS variable so we can include its header.
dnl
dnl The header files may be installed as either curses.h, or ncurses.h (would
dnl be obsolete, except that some packagers prefer this name to distinguish it
dnl from a "native" curses implementation).  If not installed for overwrite,
dnl the curses.h file would be in an ncurses subdirectory (e.g.,
dnl /usr/include/ncurses), but someone may have installed overwriting the
dnl vendor's curses.  Only very old versions (pre-1.9.2d, the first autoconf'd
dnl version) of ncurses don't define either __NCURSES_H or NCURSES_VERSION in
dnl the header.
dnl
dnl If the installer has set $CFLAGS or $CPPFLAGS so that the ncurses header
dnl is already in the include-path, don't even bother with this, since we cannot
dnl easily determine which file it is.  In this case, it has to be <curses.h>.
dnl
dnl The optional parameter gives the root name of the library, in case it is
dnl not installed as the default curses library.  That is how the
dnl wide-character version of ncurses is installed.
AC_DEFUN([CF_NCURSES_CPPFLAGS],
[AC_REQUIRE([CF_WITH_CURSES_DIR])

AC_PROVIDE([CF_CURSES_CPPFLAGS])dnl
cf_ncuhdr_root=ifelse($1,,ncurses,$1)

test -n "$cf_cv_curses_dir" && \
test "$cf_cv_curses_dir" != "no" && { \
  CF_ADD_INCDIR($cf_cv_curses_dir/include/$cf_ncuhdr_root)
}

AC_CACHE_CHECK(for $cf_ncuhdr_root header in include-path, cf_cv_ncurses_h,[
	cf_header_list="$cf_ncuhdr_root/curses.h $cf_ncuhdr_root/ncurses.h"
	{ test "$cf_ncuhdr_root" = ncurses || test "$cf_ncuhdr_root" = ncursesw; } && cf_header_list="$cf_header_list curses.h ncurses.h"
	for cf_header in $cf_header_list
	do
		CF_NCURSES_CC_CHECK(cf_cv_ncurses_h,$cf_header,$1)
		test "$cf_cv_ncurses_h" != no && break
	done
])

CF_NCURSES_HEADER
CF_TERM_HEADER

# some applications need this, but should check for NCURSES_VERSION
AC_DEFINE(NCURSES,1,[Define to 1 if we are using ncurses headers/libraries])

CF_NCURSES_VERSION
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_EXT_FUNCS version: 4 updated: 2012/10/06 16:39:58
dnl --------------------
dnl Since 2007/11/17, ncurses has defined NCURSES_EXT_FUNCS; earlier versions
dnl may provide these functions.  Define the symbol if it is not defined, and
dnl if it is valid.
AC_DEFUN([CF_NCURSES_EXT_FUNCS],
[
AC_CACHE_CHECK(for ncurses extended functions,cf_cv_ncurses_ext_funcs,[
AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>],
[
int x = NCURSES_EXT_FUNCS
],[cf_cv_ncurses_ext_funcs=defined],[
AC_TRY_LINK([
#include <${cf_cv_ncurses_header:-curses.h}>],
[
	(void) assume_default_colors (0, 0);
	(void) curses_version ();
	(void) define_key (0, 0);
	(void) is_term_resized (0, 0);
	(void) key_defined (0);
	(void) keybound (0, 0);
	(void) keyok (0, 0);
	(void) resize_term (0, 0);
	(void) resizeterm (0, 0);
	(void) use_default_colors ();
	(void) use_extended_names (0);
	(void) wresize (0, 0, 0);],
	[cf_cv_ncurses_ext_funcs=yes],
	[cf_cv_ncurses_ext_funcs=no])
])
])
test "$cf_cv_ncurses_ext_funcs" = yes && AC_DEFINE(NCURSES_EXT_FUNCS,1,[Define to 1 if we have ncurses extensions])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_HEADER version: 7 updated: 2021/01/04 19:33:05
dnl -----------------
dnl Find a "curses" header file, e.g,. "curses.h", or one of the more common
dnl variations of ncurses' installs.
dnl
dnl See also CF_CURSES_HEADER, which sets the same cache variable.
AC_DEFUN([CF_NCURSES_HEADER],[

if test "$cf_cv_ncurses_h" != no ; then
	cf_cv_ncurses_header=$cf_cv_ncurses_h
else

AC_CACHE_CHECK(for $cf_ncuhdr_root include-path, cf_cv_ncurses_h2,[
	test -n "$verbose" && echo
	CF_HEADER_PATH(cf_search,$cf_ncuhdr_root)
	test -n "$verbose" && echo "search path $cf_search"
	cf_save2_CPPFLAGS="$CPPFLAGS"
	for cf_incdir in $cf_search
	do
		CF_ADD_INCDIR($cf_incdir)
		for cf_header in \
			ncurses.h \
			curses.h
		do
			CF_NCURSES_CC_CHECK(cf_cv_ncurses_h2,$cf_header,$1)
			if test "$cf_cv_ncurses_h2" != no ; then
				cf_cv_ncurses_h2=$cf_incdir/$cf_header
				test -n "$verbose" && echo $ECHO_N "	... found $ECHO_C" 1>&AC_FD_MSG
				break
			fi
			test -n "$verbose" && echo "	... tested $cf_incdir/$cf_header" 1>&AC_FD_MSG
		done
		CPPFLAGS="$cf_save2_CPPFLAGS"
		test "$cf_cv_ncurses_h2" != no && break
	done
	test "$cf_cv_ncurses_h2" = no && AC_MSG_ERROR(not found)
	])

	CF_DIRNAME(cf_1st_incdir,$cf_cv_ncurses_h2)
	cf_cv_ncurses_header="`basename "$cf_cv_ncurses_h2"`"
	if test "`basename "$cf_1st_incdir"`" = "$cf_ncuhdr_root" ; then
		cf_cv_ncurses_header="$cf_ncuhdr_root/$cf_cv_ncurses_header"
	fi
	CF_ADD_INCDIR($cf_1st_incdir)

fi

# Set definitions to allow ifdef'ing for ncurses.h

case "$cf_cv_ncurses_header" in
(*ncurses.h)
	AC_DEFINE(HAVE_NCURSES_H,1,[Define to 1 if we have ncurses.h])
	;;
esac

case "$cf_cv_ncurses_header" in
(ncurses/curses.h|ncurses/ncurses.h)
	AC_DEFINE(HAVE_NCURSES_NCURSES_H,1,[Define to 1 if we have ncurses/ncurses.h])
	;;
(ncursesw/curses.h|ncursesw/ncurses.h)
	AC_DEFINE(HAVE_NCURSESW_NCURSES_H,1,[Define to 1 if we have ncursesw/ncurses.h])
	;;
esac

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_LIBS version: 21 updated: 2021/09/04 06:37:12
dnl ---------------
dnl Look for the ncurses library.  This is a little complicated on Linux,
dnl because it may be linked with the gpm (general purpose mouse) library.
dnl Some distributions have gpm linked with (bsd) curses, which makes it
dnl unusable with ncurses.  However, we don't want to link with gpm unless
dnl ncurses has a dependency, since gpm is normally set up as a shared library,
dnl and the linker will record a dependency.
dnl
dnl The optional parameter gives the root name of the library, in case it is
dnl not installed as the default curses library.  That is how the
dnl wide-character version of ncurses is installed.
AC_DEFUN([CF_NCURSES_LIBS],
[AC_REQUIRE([CF_NCURSES_CPPFLAGS])

cf_nculib_root=ifelse($1,,ncurses,$1)
	# This works, except for the special case where we find gpm, but
	# ncurses is in a nonstandard location via $LIBS, and we really want
	# to link gpm.
cf_ncurses_LIBS=""
cf_ncurses_SAVE="$LIBS"
AC_CHECK_LIB(gpm,Gpm_Open,
	[AC_CHECK_LIB(gpm,initscr,
		[LIBS="$cf_ncurses_SAVE"],
		[cf_ncurses_LIBS="-lgpm"])])

case "$host_os" in
(freebsd*)
	# This is only necessary if you are linking against an obsolete
	# version of ncurses (but it should do no harm, since it is static).
	if test "$cf_nculib_root" = ncurses ; then
		AC_CHECK_LIB(mytinfo,tgoto,[cf_ncurses_LIBS="-lmytinfo $cf_ncurses_LIBS"])
	fi
	;;
esac

CF_ADD_LIBS($cf_ncurses_LIBS)

if test -n "$cf_cv_curses_dir" && test "$cf_cv_curses_dir" != "no"
then
	CF_ADD_LIBS(-l$cf_nculib_root)
else
	CF_FIND_LIBRARY($cf_nculib_root,$cf_nculib_root,
		[#include <${cf_cv_ncurses_header:-curses.h}>],
		[initscr()],
		initscr)
fi

if test -n "$cf_ncurses_LIBS" ; then
	AC_MSG_CHECKING(if we can link $cf_nculib_root without $cf_ncurses_LIBS)
	cf_ncurses_SAVE="$LIBS"
	for p in $cf_ncurses_LIBS ; do
		q=`echo "$LIBS" | sed -e "s%$p %%" -e "s%$p$%%"`
		if test "$q" != "$LIBS" ; then
			LIBS="$q"
		fi
	done
	AC_TRY_LINK([#include <${cf_cv_ncurses_header:-curses.h}>],
		[initscr(); mousemask(0,0); tigetstr((char *)0);],
		[AC_MSG_RESULT(yes)],
		[AC_MSG_RESULT(no)
		 LIBS="$cf_ncurses_SAVE"])
fi

CF_UPPER(cf_nculib_ROOT,HAVE_LIB$cf_nculib_root)
AC_DEFINE_UNQUOTED($cf_nculib_ROOT)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_PTHREADS version: 2 updated: 2016/04/22 05:07:41
dnl -------------------
dnl Use this followup check to ensure that we link with pthreads if ncurses
dnl uses it.
AC_DEFUN([CF_NCURSES_PTHREADS],[
: ${cf_nculib_root:=ifelse($1,,ncurses,$1)}
AC_CHECK_LIB($cf_nculib_root,_nc_init_pthreads,
	cf_cv_ncurses_pthreads=yes,
	cf_cv_ncurses_pthreads=no)
if test "$cf_cv_ncurses_pthreads" = yes
then
	CF_ADD_LIBS(-lpthread)
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_VERSION version: 16 updated: 2020/12/31 20:19:42
dnl ------------------
dnl Check for the version of ncurses, to aid in reporting bugs, etc.
dnl Call CF_CURSES_CPPFLAGS first, or CF_NCURSES_CPPFLAGS.  We don't use
dnl AC_REQUIRE since that does not work with the shell's if/then/else/fi.
AC_DEFUN([CF_NCURSES_VERSION],
[
AC_REQUIRE([CF_CURSES_CPPFLAGS])dnl
AC_CACHE_CHECK(for ncurses version, cf_cv_ncurses_version,[
	cf_cv_ncurses_version=no
	cf_tempfile=out$$
	rm -f "$cf_tempfile"
	AC_TRY_RUN([
#include <${cf_cv_ncurses_header:-curses.h}>
#include <stdio.h>
int main(void)
{
	FILE *fp = fopen("$cf_tempfile", "w");
#ifdef NCURSES_VERSION
# ifdef NCURSES_VERSION_PATCH
	fprintf(fp, "%s.%d\\n", NCURSES_VERSION, NCURSES_VERSION_PATCH);
# else
	fprintf(fp, "%s\\n", NCURSES_VERSION);
# endif
#else
# ifdef __NCURSES_H
	fprintf(fp, "old\\n");
# else
	make an error
# endif
#endif
	${cf_cv_main_return:-return}(0);
}],[
	cf_cv_ncurses_version=`cat $cf_tempfile`],,[

	# This will not work if the preprocessor splits the line after the
	# Autoconf token.  The 'unproto' program does that.
	cat > "conftest.$ac_ext" <<EOF
#include <${cf_cv_ncurses_header:-curses.h}>
#undef Autoconf
#ifdef NCURSES_VERSION
Autoconf NCURSES_VERSION
#else
#ifdef __NCURSES_H
Autoconf "old"
#endif
;
#endif
EOF
	cf_try="$ac_cpp conftest.$ac_ext 2>&AC_FD_CC | grep '^Autoconf ' >conftest.out"
	AC_TRY_EVAL(cf_try)
	if test -f conftest.out ; then
		cf_out=`sed -e 's%^Autoconf %%' -e 's%^[[^"]]*"%%' -e 's%".*%%' conftest.out`
		test -n "$cf_out" && cf_cv_ncurses_version="$cf_out"
		rm -f conftest.out
	fi
])
	rm -f "$cf_tempfile"
])
test "$cf_cv_ncurses_version" = no || AC_DEFINE(NCURSES,1,[Define to 1 if we are using ncurses headers/libraries])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NCURSES_WRAP_PREFIX version: 1 updated: 2009/03/28 16:08:10
dnl ----------------------
dnl Check for ncurses "wrap-prefix" used for public variables which have been
dnl wrapped with a function to help with concurrency control.
AC_DEFUN([CF_NCURSES_WRAP_PREFIX],
[
AC_MSG_CHECKING(for ncurses wrap-prefix)
AC_ARG_WITH(ncurses-wrap-prefix,
	[  --with-ncurses-wrap-prefix naming-prefix for ncurses wrapped-variables],
	[NCURSES_WRAP_PREFIX=$withval],
	[NCURSES_WRAP_PREFIX=_nc_])
AC_MSG_RESULT($NCURSES_WRAP_PREFIX)

AC_SUBST(NCURSES_WRAP_PREFIX)
])
dnl ---------------------------------------------------------------------------
dnl CF_NETBSD_FORM_H version: 2 updated: 2012/10/06 16:39:58
dnl ----------------
dnl Check for NetBSD's form.h, which is incompatible with SVr4 and ncurses.
dnl Some workarounds are needed in client programs to allow them to compile.
AC_DEFUN([CF_NETBSD_FORM_H],[
AC_CACHE_CHECK(for NetBSD form.h,cf_cv_netbsd_form_h,[
AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>
#include <form.h>
],[
	FORM *form;
	int y = current_field(form)->cursor_ypos;
	int x = current_field(form)->cursor_xpos;
],[cf_cv_netbsd_form_h=yes
],[cf_cv_netbsd_form_h=no])
])

test "$cf_cv_netbsd_form_h" = yes && AC_DEFINE(HAVE_NETBSD_FORM_H,1,[Define to 1 if we appear to be using NetBSD form.h])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NETBSD_MENU_H version: 2 updated: 2012/10/06 16:39:58
dnl ----------------
dnl Check for NetBSD's menu.h, which is incompatible with SVr4 and ncurses.
dnl Some workarounds are needed in client programs to allow them to compile.
AC_DEFUN([CF_NETBSD_MENU_H],[
AC_CACHE_CHECK(for NetBSD menu.h,cf_cv_netbsd_menu_h,[
AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>
#include <menu.h>
],[
	MENU *menu;
	int y = menu->max_item_width;
],[cf_cv_netbsd_menu_h=yes
],[cf_cv_netbsd_menu_h=no])
])

test "$cf_cv_netbsd_menu_h" = yes && AC_DEFINE(HAVE_NETBSD_MENU_H,1,[Define to 1 if we appear to be using NetBSD menu.h])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_NO_LEAKS_OPTION version: 9 updated: 2021/06/13 19:45:41
dnl ------------------
dnl see CF_WITH_NO_LEAKS
dnl
dnl $1 = option/name
dnl $2 = help-text
dnl $3 = symbol to define if the option is set
dnl $4 = additional actions to take if the option is set
AC_DEFUN([CF_NO_LEAKS_OPTION],[
AC_MSG_CHECKING(if you want to use $1 for testing)
AC_ARG_WITH($1,
	[$2],
	[case "x$withval" in
	(x|xno) ;;
	(*)
		: "${with_cflags:=-g}"
		: "${enable_leaks:=no}"
		with_$1=yes
		AC_DEFINE_UNQUOTED($3,1,"Define to 1 if you want to use $1 for testing.")ifelse([$4],,[
	 $4
])
		;;
	esac],
	[with_$1=])
AC_MSG_RESULT(${with_$1:-no})

case ".$with_cflags" in
(.*-g*)
	case .$CFLAGS in
	(.*-g*)
		;;
	(*)
		CF_ADD_CFLAGS([-g])
		;;
	esac
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PATH_SYNTAX version: 18 updated: 2020/12/31 18:40:20
dnl --------------
dnl Check the argument to see that it looks like a pathname.  Rewrite it if it
dnl begins with one of the prefix/exec_prefix variables, and then again if the
dnl result begins with 'NONE'.  This is necessary to work around autoconf's
dnl delayed evaluation of those symbols.
AC_DEFUN([CF_PATH_SYNTAX],[
if test "x$prefix" != xNONE; then
	cf_path_syntax="$prefix"
else
	cf_path_syntax="$ac_default_prefix"
fi

case ".[$]$1" in
(.\[$]\(*\)*|.\'*\'*)
	;;
(..|./*|.\\*)
	;;
(.[[a-zA-Z]]:[[\\/]]*) # OS/2 EMX
	;;
(.\[$]\{*prefix\}*|.\[$]\{*dir\}*)
	eval $1="[$]$1"
	case ".[$]$1" in
	(.NONE/*)
		$1=`echo "[$]$1" | sed -e s%NONE%$cf_path_syntax%`
		;;
	esac
	;;
(.no|.NONE/*)
	$1=`echo "[$]$1" | sed -e s%NONE%$cf_path_syntax%`
	;;
(*)
	ifelse([$2],,[AC_MSG_ERROR([expected a pathname, not \"[$]$1\"])],$2)
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PDCURSES_X11 version: 15 updated: 2021/01/02 09:31:20
dnl ---------------
dnl Configure for PDCurses' X11 library
AC_DEFUN([CF_PDCURSES_X11],[
AC_REQUIRE([CF_X_ATHENA])

CF_ACVERSION_CHECK(2.52,
	[AC_CHECK_TOOLS(XCURSES_CONFIG, xcurses-config, none)],
	[AC_PATH_PROGS(XCURSES_CONFIG, xcurses-config, none)])

if test "$XCURSES_CONFIG" != none ; then

CF_ADD_CFLAGS(`$XCURSES_CONFIG --cflags`)
CF_ADD_LIBS(`$XCURSES_CONFIG --libs`)

cf_cv_lib_XCurses=yes

else

LDFLAGS="$LDFLAGS $X_LIBS"
CF_CHECK_CFLAGS($X_CFLAGS)
AC_CHECK_LIB(X11,XOpenDisplay,
	[CF_ADD_LIBS(-lX11)],,
	[$X_PRE_LIBS $LIBS $X_EXTRA_LIBS])
AC_CACHE_CHECK(for XCurses library,cf_cv_lib_XCurses,[
CF_ADD_LIBS(-lXCurses)
AC_TRY_LINK([
#include <xcurses.h>
char *XCursesProgramName = "test";
],[XCursesExit();],
[cf_cv_lib_XCurses=yes],
[cf_cv_lib_XCurses=no])
])

fi

if test "$cf_cv_lib_XCurses" = yes ; then
	AC_DEFINE(UNIX,1,[Define to 1 if using PDCurses on Unix])
	AC_DEFINE(XCURSES,1,[Define to 1 if using PDCurses on Unix])
	AC_CHECK_HEADER(xcurses.h, AC_DEFINE(HAVE_XCURSES,1,[Define to 1 if using PDCurses on Unix]))
else
	AC_MSG_ERROR(Cannot link with XCurses)
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PKG_CONFIG version: 12 updated: 2021/10/10 20:18:09
dnl -------------
dnl Check for the package-config program, unless disabled by command-line.
dnl
dnl Sets $PKG_CONFIG to the pathname of the pkg-config program.
AC_DEFUN([CF_PKG_CONFIG],
[
AC_MSG_CHECKING(if you want to use pkg-config)
AC_ARG_WITH(pkg-config,
	[  --with-pkg-config{=path} enable/disable use of pkg-config],
	[cf_pkg_config=$withval],
	[cf_pkg_config=yes])
AC_MSG_RESULT($cf_pkg_config)

case "$cf_pkg_config" in
(no)
	PKG_CONFIG=none
	;;
(yes)
	CF_ACVERSION_CHECK(2.52,
		[AC_PATH_TOOL(PKG_CONFIG, pkg-config, none)],
		[AC_PATH_PROG(PKG_CONFIG, pkg-config, none)])
	;;
(*)
	PKG_CONFIG=$withval
	;;
esac

test -z "$PKG_CONFIG" && PKG_CONFIG=none
if test "$PKG_CONFIG" != none ; then
	CF_PATH_SYNTAX(PKG_CONFIG)
elif test "x$cf_pkg_config" != xno ; then
	AC_MSG_WARN(pkg-config is not installed)
fi

AC_SUBST(PKG_CONFIG)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_POSIX_C_SOURCE version: 11 updated: 2018/12/31 20:46:17
dnl -----------------
dnl Define _POSIX_C_SOURCE to the given level, and _POSIX_SOURCE if needed.
dnl
dnl	POSIX.1-1990				_POSIX_SOURCE
dnl	POSIX.1-1990 and			_POSIX_SOURCE and
dnl		POSIX.2-1992 C-Language			_POSIX_C_SOURCE=2
dnl		Bindings Option
dnl	POSIX.1b-1993				_POSIX_C_SOURCE=199309L
dnl	POSIX.1c-1996				_POSIX_C_SOURCE=199506L
dnl	X/Open 2000				_POSIX_C_SOURCE=200112L
dnl
dnl Parameters:
dnl	$1 is the nominal value for _POSIX_C_SOURCE
AC_DEFUN([CF_POSIX_C_SOURCE],
[AC_REQUIRE([CF_POSIX_VISIBLE])dnl

if test "$cf_cv_posix_visible" = no; then

cf_POSIX_C_SOURCE=ifelse([$1],,199506L,[$1])

cf_save_CFLAGS="$CFLAGS"
cf_save_CPPFLAGS="$CPPFLAGS"

CF_REMOVE_DEFINE(cf_trim_CFLAGS,$cf_save_CFLAGS,_POSIX_C_SOURCE)
CF_REMOVE_DEFINE(cf_trim_CPPFLAGS,$cf_save_CPPFLAGS,_POSIX_C_SOURCE)

AC_CACHE_CHECK(if we should define _POSIX_C_SOURCE,cf_cv_posix_c_source,[
	CF_MSG_LOG(if the symbol is already defined go no further)
	AC_TRY_COMPILE([#include <sys/types.h>],[
#ifndef _POSIX_C_SOURCE
make an error
#endif],
	[cf_cv_posix_c_source=no],
	[cf_want_posix_source=no
	 case .$cf_POSIX_C_SOURCE in
	 (.[[12]]??*)
		cf_cv_posix_c_source="-D_POSIX_C_SOURCE=$cf_POSIX_C_SOURCE"
		;;
	 (.2)
		cf_cv_posix_c_source="-D_POSIX_C_SOURCE=$cf_POSIX_C_SOURCE"
		cf_want_posix_source=yes
		;;
	 (.*)
		cf_want_posix_source=yes
		;;
	 esac
	 if test "$cf_want_posix_source" = yes ; then
		AC_TRY_COMPILE([#include <sys/types.h>],[
#ifdef _POSIX_SOURCE
make an error
#endif],[],
		cf_cv_posix_c_source="$cf_cv_posix_c_source -D_POSIX_SOURCE")
	 fi
	 CF_MSG_LOG(ifdef from value $cf_POSIX_C_SOURCE)
	 CFLAGS="$cf_trim_CFLAGS"
	 CPPFLAGS="$cf_trim_CPPFLAGS"
	 CF_APPEND_TEXT(CPPFLAGS,$cf_cv_posix_c_source)
	 CF_MSG_LOG(if the second compile does not leave our definition intact error)
	 AC_TRY_COMPILE([#include <sys/types.h>],[
#ifndef _POSIX_C_SOURCE
make an error
#endif],,
	 [cf_cv_posix_c_source=no])
	 CFLAGS="$cf_save_CFLAGS"
	 CPPFLAGS="$cf_save_CPPFLAGS"
	])
])

if test "$cf_cv_posix_c_source" != no ; then
	CFLAGS="$cf_trim_CFLAGS"
	CPPFLAGS="$cf_trim_CPPFLAGS"
	CF_ADD_CFLAGS($cf_cv_posix_c_source)
fi

fi # cf_cv_posix_visible

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_POSIX_VISIBLE version: 1 updated: 2018/12/31 20:46:17
dnl ----------------
dnl POSIX documents test-macros which an application may set before any system
dnl headers are included to make features available.
dnl
dnl Some BSD platforms (originally FreeBSD, but copied by a few others)
dnl diverged from POSIX in 2002 by setting symbols which make all of the most
dnl recent features visible in the system header files unless the application
dnl overrides the corresponding test-macros.  Doing that introduces portability
dnl problems.
dnl
dnl This macro makes a special check for the symbols used for this, to avoid a
dnl conflicting definition.
AC_DEFUN([CF_POSIX_VISIBLE],
[
AC_CACHE_CHECK(if the POSIX test-macros are already defined,cf_cv_posix_visible,[
AC_TRY_COMPILE([#include <stdio.h>],[
#if defined(__POSIX_VISIBLE) && ((__POSIX_VISIBLE - 0L) > 0) \
	&& defined(__XSI_VISIBLE) && ((__XSI_VISIBLE - 0L) > 0) \
	&& defined(__BSD_VISIBLE) && ((__BSD_VISIBLE - 0L) > 0) \
	&& defined(__ISO_C_VISIBLE) && ((__ISO_C_VISIBLE - 0L) > 0)
#error conflicting symbols found
#endif
],[cf_cv_posix_visible=no],[cf_cv_posix_visible=yes])
])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PRG_RULES version: 2 updated: 2021/01/01 13:31:04
dnl ------------
dnl Append definitions and rules for the given programs to the subdirectory
dnl Makefiles, and the recursion rule for the top-level Makefile.
dnl
dnl parameters
dnl	$1 = script to run
dnl	$2 = list of subdirectories
dnl
dnl variables
dnl	$AWK
AC_DEFUN([CF_PRG_RULES],
[
for cf_dir in $2
do
	if test ! -d "$srcdir/$cf_dir" ; then
		continue
	elif test -f "$srcdir/$cf_dir/programs" ; then
		$AWK -f $1 "$srcdir/$cf_dir/programs" >>$cf_dir/Makefile
	fi
done

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PROG_CC version: 5 updated: 2019/12/31 08:53:54
dnl ----------
dnl standard check for CC, plus followup sanity checks
dnl $1 = optional parameter to pass to AC_PROG_CC to specify compiler name
AC_DEFUN([CF_PROG_CC],[
CF_ACVERSION_CHECK(2.53,
	[AC_MSG_WARN(this will incorrectly handle gnatgcc choice)
	 AC_REQUIRE([AC_PROG_CC])],
	[])
ifelse($1,,[AC_PROG_CC],[AC_PROG_CC($1)])
CF_GCC_VERSION
CF_ACVERSION_CHECK(2.52,
	[AC_PROG_CC_STDC],
	[CF_ANSI_CC_REQD])
CF_CC_ENV_FLAGS
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PROG_INSTALL version: 10 updated: 2021/01/04 19:33:05
dnl ---------------
dnl Force $INSTALL to be an absolute-path.  Otherwise, edit_man.sh and the
dnl misc/tabset install won't work properly.  Usually this happens only when
dnl using the fallback mkinstalldirs script
AC_DEFUN([CF_PROG_INSTALL],
[AC_PROG_INSTALL
case $INSTALL in
(/*)
	;;
(*)
	CF_DIRNAME(cf_dir,$INSTALL)
	test -z "$cf_dir" && cf_dir=.
	INSTALL="`cd \"$cf_dir\" && pwd`"/"`echo "$INSTALL" | sed -e 's%^.*/%%'`"
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_PROG_LINT version: 4 updated: 2019/11/20 18:55:37
dnl ------------
AC_DEFUN([CF_PROG_LINT],
[
AC_CHECK_PROGS(LINT, lint cppcheck splint)
case "x$LINT" in
(xcppcheck|x*/cppcheck)
	test -z "$LINT_OPTS" && LINT_OPTS="--enable=all"
	;;
esac
AC_SUBST(LINT_OPTS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_REMOVE_CFLAGS version: 3 updated: 2021/09/05 17:25:40
dnl ----------------
dnl Remove a given option from CFLAGS/CPPFLAGS
dnl $1 = option to remove
dnl $2 = variable to update
dnl $3 = nonempty to allow verbose message
define([CF_REMOVE_CFLAGS],
[
cf_tmp_cflag=`echo "x$1" | sed -e 's/^.//' -e 's/=.*//'`
while true
do
	cf_old_cflag=`echo "x[$]$2" | sed -e 's/^.//' -e 's/[[ 	]][[ 	]]*-/ -/g' -e "s%$cf_tmp_cflag\\(=[[^ 	]][[^ 	]]*\\)\?%%" -e 's/^[[ 	]]*//' -e 's%[[ ]][[ ]]*-D% -D%g' -e 's%[[ ]][[ ]]*-I% -I%g'`
	test "[$]$2" != "$cf_old_cflag" || break
	ifelse([$3],,,[CF_VERBOSE(removing old option $1 from $2)])
	$2="$cf_old_cflag"
done
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_REMOVE_DEFINE version: 3 updated: 2010/01/09 11:05:50
dnl ----------------
dnl Remove all -U and -D options that refer to the given symbol from a list
dnl of C compiler options.  This works around the problem that not all
dnl compilers process -U and -D options from left-to-right, so a -U option
dnl cannot be used to cancel the effect of a preceding -D option.
dnl
dnl $1 = target (which could be the same as the source variable)
dnl $2 = source (including '$')
dnl $3 = symbol to remove
define([CF_REMOVE_DEFINE],
[
$1=`echo "$2" | \
	sed	-e 's/-[[UD]]'"$3"'\(=[[^ 	]]*\)\?[[ 	]]/ /g' \
		-e 's/-[[UD]]'"$3"'\(=[[^ 	]]*\)\?[$]//g'`
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_RESTORE_XTRA_FLAGS version: 1 updated: 2020/01/11 16:47:45
dnl ---------------------
dnl Restore flags saved in CF_SAVE_XTRA_FLAGS
dnl $1 = name of current macro
define([CF_RESTORE_XTRA_FLAGS],
[
LIBS="$cf_save_LIBS_$1"
CFLAGS="$cf_save_CFLAGS_$1"
CPPFLAGS="$cf_save_CPPFLAGS_$1"
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_RPATH_HACK version: 13 updated: 2021/01/03 18:30:50
dnl -------------
AC_DEFUN([CF_RPATH_HACK],
[AC_REQUIRE([AC_PROG_FGREP])dnl
AC_REQUIRE([CF_LD_RPATH_OPT])dnl

AC_MSG_CHECKING(for updated LDFLAGS)
if test -n "$LD_RPATH_OPT" ; then
	AC_MSG_RESULT(maybe)

	AC_CHECK_PROGS(cf_ldd_prog,ldd,no)
	cf_rpath_list="/usr/lib /lib"
	if test "$cf_ldd_prog" != no
	then
		cf_rpath_oops=

AC_TRY_LINK([#include <stdio.h>],
		[printf("Hello");],
		[cf_rpath_oops=`"$cf_ldd_prog" "conftest$ac_exeext" | ${FGREP-fgrep} ' not found' | sed -e 's% =>.*$%%' |sort | uniq`
		 cf_rpath_list=`"$cf_ldd_prog" "conftest$ac_exeext" | ${FGREP-fgrep} / | sed -e 's%^.*[[ 	]]/%/%' -e 's%/[[^/]][[^/]]*$%%' |sort | uniq`])

		# If we passed the link-test, but get a "not found" on a given library,
		# this could be due to inept reconfiguration of gcc to make it only
		# partly honor /usr/local/lib (or whatever).  Sometimes this behavior
		# is intentional, e.g., installing gcc in /usr/bin and suppressing the
		# /usr/local libraries.
		if test -n "$cf_rpath_oops"
		then
			for cf_rpath_src in $cf_rpath_oops
			do
				for cf_rpath_dir in \
					/usr/local \
					/usr/pkg \
					/opt/sfw
				do
					if test -f "$cf_rpath_dir/lib/$cf_rpath_src"
					then
						CF_VERBOSE(...adding -L$cf_rpath_dir/lib to LDFLAGS for $cf_rpath_src)
						LDFLAGS="$LDFLAGS -L$cf_rpath_dir/lib"
						break
					fi
				done
			done
		fi
	fi

	CF_VERBOSE(...checking EXTRA_LDFLAGS $EXTRA_LDFLAGS)

	CF_RPATH_HACK_2(LDFLAGS)
	CF_RPATH_HACK_2(LIBS)

	CF_VERBOSE(...checked EXTRA_LDFLAGS $EXTRA_LDFLAGS)
else
	AC_MSG_RESULT(no)
fi
AC_SUBST(EXTRA_LDFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_RPATH_HACK_2 version: 8 updated: 2021/01/01 13:31:04
dnl ---------------
dnl Do one set of substitutions for CF_RPATH_HACK, adding an rpath option to
dnl EXTRA_LDFLAGS for each -L option found.
dnl
dnl $cf_rpath_list contains a list of directories to ignore.
dnl
dnl $1 = variable name to update.  The LDFLAGS variable should be the only one,
dnl      but LIBS often has misplaced -L options.
AC_DEFUN([CF_RPATH_HACK_2],
[
CF_VERBOSE(...checking $1 [$]$1)

cf_rpath_dst=
for cf_rpath_src in [$]$1
do
	case "$cf_rpath_src" in
	(-L*)

		# check if this refers to a directory which we will ignore
		cf_rpath_skip=no
		if test -n "$cf_rpath_list"
		then
			for cf_rpath_item in $cf_rpath_list
			do
				if test "x$cf_rpath_src" = "x-L$cf_rpath_item"
				then
					cf_rpath_skip=yes
					break
				fi
			done
		fi

		if test "$cf_rpath_skip" = no
		then
			# transform the option
			if test "$LD_RPATH_OPT" = "-R " ; then
				cf_rpath_tmp=`echo "$cf_rpath_src" |sed -e "s%-L%-R %"`
			else
				cf_rpath_tmp=`echo "$cf_rpath_src" |sed -e "s%-L%$LD_RPATH_OPT%"`
			fi

			# if we have not already added this, add it now
			cf_rpath_tst=`echo "$EXTRA_LDFLAGS" | sed -e "s%$cf_rpath_tmp %%"`
			if test "x$cf_rpath_tst" = "x$EXTRA_LDFLAGS"
			then
				CF_VERBOSE(...Filter $cf_rpath_src ->$cf_rpath_tmp)
				EXTRA_LDFLAGS="$cf_rpath_tmp $EXTRA_LDFLAGS"
			fi
		fi
		;;
	esac
	cf_rpath_dst="$cf_rpath_dst $cf_rpath_src"
done
$1=$cf_rpath_dst

CF_VERBOSE(...checked $1 [$]$1)
AC_SUBST(EXTRA_LDFLAGS)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_SAVE_XTRA_FLAGS version: 1 updated: 2020/01/11 16:46:44
dnl ------------------
dnl Use this macro to save CFLAGS/CPPFLAGS/LIBS before checks against X headers
dnl and libraries which do not update those variables.
dnl
dnl $1 = name of current macro
define([CF_SAVE_XTRA_FLAGS],
[
cf_save_LIBS_$1="$LIBS"
cf_save_CFLAGS_$1="$CFLAGS"
cf_save_CPPFLAGS_$1="$CPPFLAGS"
LIBS="$LIBS ${X_PRE_LIBS} ${X_LIBS} ${X_EXTRA_LIBS}"
for cf_X_CFLAGS in $X_CFLAGS
do
	case "x$cf_X_CFLAGS" in
	x-[[IUD]]*)
		CPPFLAGS="$CPPFLAGS $cf_X_CFLAGS"
		;;
	*)
		CFLAGS="$CFLAGS $cf_X_CFLAGS"
		;;
	esac
done
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_SIGWINCH version: 6 updated: 2021/01/01 13:31:04
dnl -----------
dnl Use this macro after CF_XOPEN_SOURCE, but do not require it (not all
dnl programs need this test).
dnl
dnl This is really a Mac OS X 10.4.3 workaround.  Defining _POSIX_C_SOURCE
dnl forces SIGWINCH to be undefined (breaks xterm, ncurses).  Oddly, the struct
dnl winsize declaration is left alone - we may revisit this if Apple choose to
dnl break that part of the interface as well.
AC_DEFUN([CF_SIGWINCH],
[
AC_CACHE_CHECK(if SIGWINCH is defined,cf_cv_define_sigwinch,[
	AC_TRY_COMPILE([
#include <sys/types.h>
#include <sys/signal.h>
],[int x = SIGWINCH; (void)x],
	[cf_cv_define_sigwinch=yes],
	[AC_TRY_COMPILE([
#undef _XOPEN_SOURCE
#undef _POSIX_SOURCE
#undef _POSIX_C_SOURCE
#include <sys/types.h>
#include <sys/signal.h>
],[int x = SIGWINCH; (void)x],
	[cf_cv_define_sigwinch=maybe],
	[cf_cv_define_sigwinch=no])
])
])

if test "$cf_cv_define_sigwinch" = maybe ; then
AC_CACHE_CHECK(for actual SIGWINCH definition,cf_cv_fixup_sigwinch,[
cf_cv_fixup_sigwinch=unknown
cf_sigwinch=32
while test "$cf_sigwinch" != 1
do
	AC_TRY_COMPILE([
#undef _XOPEN_SOURCE
#undef _POSIX_SOURCE
#undef _POSIX_C_SOURCE
#include <sys/types.h>
#include <sys/signal.h>
],[
#if SIGWINCH != $cf_sigwinch
make an error
#endif
int x = SIGWINCH; (void)x],
	[cf_cv_fixup_sigwinch=$cf_sigwinch
	 break])

cf_sigwinch="`expr "$cf_sigwinch" - 1`"
done
])

	if test "$cf_cv_fixup_sigwinch" != unknown ; then
		CPPFLAGS="$CPPFLAGS -DSIGWINCH=$cf_cv_fixup_sigwinch"
	fi
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_SIG_ATOMIC_T version: 5 updated: 2020/03/10 18:53:47
dnl ---------------
dnl signal handler, but there are some gcc dependencies in that recommendation.
dnl Try anyway.
AC_DEFUN([CF_SIG_ATOMIC_T],
[
AC_MSG_CHECKING(for signal global datatype)
AC_CACHE_VAL(cf_cv_sig_atomic_t,[
	for cf_type in \
		"volatile sig_atomic_t" \
		"sig_atomic_t" \
		"int"
	do
	AC_TRY_COMPILE([
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>

extern $cf_type x;
$cf_type x;
static void handler(int sig)
{
	(void)sig;
	x = 5;
}],
		[signal(SIGINT, handler);
		 x = 1],
		[cf_cv_sig_atomic_t=$cf_type],
		[cf_cv_sig_atomic_t=no])
		test "$cf_cv_sig_atomic_t" != no && break
	done
	])
AC_MSG_RESULT($cf_cv_sig_atomic_t)
test "$cf_cv_sig_atomic_t" != no && AC_DEFINE_UNQUOTED(SIG_ATOMIC_T, $cf_cv_sig_atomic_t,[Define to signal global datatype])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_SUBDIR_PATH version: 7 updated: 2014/12/04 04:33:06
dnl --------------
dnl Construct a search-list for a nonstandard header/lib-file
dnl	$1 = the variable to return as result
dnl	$2 = the package name
dnl	$3 = the subdirectory, e.g., bin, include or lib
AC_DEFUN([CF_SUBDIR_PATH],
[
$1=

CF_ADD_SUBDIR_PATH($1,$2,$3,$prefix,NONE)

for cf_subdir_prefix in \
	/usr \
	/usr/local \
	/usr/pkg \
	/opt \
	/opt/local \
	[$]HOME
do
	CF_ADD_SUBDIR_PATH($1,$2,$3,$cf_subdir_prefix,$prefix)
done
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_SYS_TIME_SELECT version: 6 updated: 2015/04/18 08:56:57
dnl ------------------
dnl Check if we can include <sys/time.h> with <sys/select.h>; this breaks on
dnl older SCO configurations.
AC_DEFUN([CF_SYS_TIME_SELECT],
[
AC_MSG_CHECKING(if sys/time.h works with sys/select.h)
AC_CACHE_VAL(cf_cv_sys_time_select,[
AC_TRY_COMPILE([
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
],[],[cf_cv_sys_time_select=yes],
	 [cf_cv_sys_time_select=no])
	 ])
AC_MSG_RESULT($cf_cv_sys_time_select)
test "$cf_cv_sys_time_select" = yes && AC_DEFINE(HAVE_SYS_TIME_SELECT,1,[Define to 1 if we can include <sys/time.h> with <sys/select.h>])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_TERM_HEADER version: 6 updated: 2021/01/02 09:31:20
dnl --------------
dnl Look for term.h, which is part of X/Open curses.  It defines the interface
dnl to terminfo database.  Usually it is in the same include-path as curses.h,
dnl but some packagers change this, breaking various applications.
AC_DEFUN([CF_TERM_HEADER],[
AC_CACHE_CHECK(for terminfo header, cf_cv_term_header,[
case "${cf_cv_ncurses_header}" in
(*/ncurses.h|*/ncursesw.h)
	cf_term_header=`echo "$cf_cv_ncurses_header" | sed -e 's%ncurses[[^.]]*\.h$%term.h%'`
	;;
(*)
	cf_term_header=term.h
	;;
esac

for cf_test in $cf_term_header "ncurses/term.h" "ncursesw/term.h"
do
AC_TRY_COMPILE([#include <stdio.h>
#include <${cf_cv_ncurses_header:-curses.h}>
#include <$cf_test>
],[int x = auto_left_margin; (void)x],[
	cf_cv_term_header="$cf_test"],[
	cf_cv_term_header=unknown
	])
	test "$cf_cv_term_header" != unknown && break
done
])

# Set definitions to allow ifdef'ing to accommodate subdirectories

case "$cf_cv_term_header" in
(*term.h)
	AC_DEFINE(HAVE_TERM_H,1,[Define to 1 if we have term.h])
	;;
esac

case "$cf_cv_term_header" in
(ncurses/term.h)
	AC_DEFINE(HAVE_NCURSES_TERM_H,1,[Define to 1 if we have ncurses/term.h])
	;;
(ncursesw/term.h)
	AC_DEFINE(HAVE_NCURSESW_TERM_H,1,[Define to 1 if we have ncursesw/term.h])
	;;
esac
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_TOP_BUILDDIR version: 2 updated: 2013/07/27 17:38:32
dnl ---------------
dnl Define a top_builddir symbol, for applications that need an absolute path.
AC_DEFUN([CF_TOP_BUILDDIR],
[
top_builddir=ifelse($1,,`pwd`,$1)
AC_SUBST(top_builddir)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_TPUTS_PROTO version: 4 updated: 2021/01/04 19:45:09
dnl --------------
dnl Check for type of function-pointer passed to tputs.  Some old
dnl implementations used functions that had different prototypes, making it
dnl hard to compile portable programs using tputs.
AC_DEFUN([CF_TPUTS_PROTO],[
CF_CURSES_FUNCS(tputs)
if test "x$cf_cv_func_tputs" = xyes
then
	cf_done=no
	for cf_arg in int char
	do
		for cf_ret in int void
		do
			if test "$cf_ret" = void
			then
				cf_return="/* nothing */"
			else
				cf_return="return value"
			fi
			AC_TRY_COMPILE([
#include <${cf_cv_ncurses_header:-curses.h}>
#include <$cf_cv_term_header>

static $cf_ret outc($cf_arg value) { $cf_return; }
],[
	tputs("hello", 0, outc);
	${cf_cv_main_return:-return}(0);
],[
		CF_VERBOSE([prototype $cf_ret func($cf_arg value)])
		cat >>confdefs.h <<EOF
#define TPUTS_ARG               $cf_arg
#define TPUTS_PROTO(func,value) $cf_ret func(TPUTS_ARG value)
#define TPUTS_RETURN(value)     $cf_return
EOF
		cf_done=yes
		break
])
		done
		test "$cf_done" = yes && break
	done
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_TRIM_X_LIBS version: 3 updated: 2015/04/12 15:39:00
dnl --------------
dnl Trim extra base X libraries added as a workaround for inconsistent library
dnl dependencies returned by "new" pkg-config files.
AC_DEFUN([CF_TRIM_X_LIBS],[
	for cf_trim_lib in Xmu Xt X11
	do
		case "$LIBS" in
		(*-l$cf_trim_lib\ *-l$cf_trim_lib*)
			LIBS=`echo "$LIBS " | sed -e 's/  / /g' -e 's%-l'"$cf_trim_lib"' %%' -e 's/ $//'`
			CF_VERBOSE(..trimmed $LIBS)
			;;
		esac
	done
])
dnl ---------------------------------------------------------------------------
dnl CF_TRY_PKG_CONFIG version: 6 updated: 2020/12/31 10:54:15
dnl -----------------
dnl This is a simple wrapper to use for pkg-config, for libraries which may be
dnl available in that form.
dnl
dnl $1 = package name, which may be a shell variable
dnl $2 = extra logic to use, if any, after updating CFLAGS and LIBS
dnl $3 = logic to use if pkg-config does not have the package
AC_DEFUN([CF_TRY_PKG_CONFIG],[
AC_REQUIRE([CF_PKG_CONFIG])

if test "$PKG_CONFIG" != none && "$PKG_CONFIG" --exists "$1"; then
	CF_VERBOSE(found package $1)
	cf_pkgconfig_incs="`$PKG_CONFIG --cflags "$1" 2>/dev/null`"
	cf_pkgconfig_libs="`$PKG_CONFIG --libs   "$1" 2>/dev/null`"
	CF_VERBOSE(package $1 CFLAGS: $cf_pkgconfig_incs)
	CF_VERBOSE(package $1 LIBS: $cf_pkgconfig_libs)
	CF_ADD_CFLAGS($cf_pkgconfig_incs)
	CF_ADD_LIBS($cf_pkgconfig_libs)
	ifelse([$2],,:,[$2])
else
	cf_pkgconfig_incs=
	cf_pkgconfig_libs=
	ifelse([$3],,:,[$3])
fi
])
dnl ---------------------------------------------------------------------------
dnl CF_TRY_XOPEN_SOURCE version: 3 updated: 2021/08/28 15:20:37
dnl -------------------
dnl If _XOPEN_SOURCE is not defined in the compile environment, check if we
dnl can define it successfully.
AC_DEFUN([CF_TRY_XOPEN_SOURCE],[
AC_CACHE_CHECK(if we should define _XOPEN_SOURCE,cf_cv_xopen_source,[
	AC_TRY_COMPILE([
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
],[
#ifndef _XOPEN_SOURCE
make an error
#endif],
	[cf_cv_xopen_source=no],
	[cf_save="$CPPFLAGS"
	 CF_APPEND_TEXT(CPPFLAGS,-D_XOPEN_SOURCE=$cf_XOPEN_SOURCE)
	 AC_TRY_COMPILE([
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
],[
#ifdef _XOPEN_SOURCE
make an error
#endif],
	[cf_cv_xopen_source=no],
	[cf_cv_xopen_source=$cf_XOPEN_SOURCE])
	CPPFLAGS="$cf_save"
	])
])

if test "$cf_cv_xopen_source" != no ; then
	CF_REMOVE_DEFINE(CFLAGS,$CFLAGS,_XOPEN_SOURCE)
	CF_REMOVE_DEFINE(CPPFLAGS,$CPPFLAGS,_XOPEN_SOURCE)
	cf_temp_xopen_source="-D_XOPEN_SOURCE=$cf_cv_xopen_source"
	CF_APPEND_CFLAGS($cf_temp_xopen_source)
fi
])
dnl ---------------------------------------------------------------------------
dnl CF_UPPER version: 5 updated: 2001/01/29 23:40:59
dnl --------
dnl Make an uppercase version of a variable
dnl $1=uppercase($2)
AC_DEFUN([CF_UPPER],
[
$1=`echo "$2" | sed y%abcdefghijklmnopqrstuvwxyz./-%ABCDEFGHIJKLMNOPQRSTUVWXYZ___%`
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_UTF8_LIB version: 9 updated: 2021/05/19 19:35:25
dnl -----------
dnl Check for multibyte support, and if not found, utf8 compatibility library
AC_DEFUN([CF_UTF8_LIB],
[
AC_HAVE_HEADERS(wchar.h)
AC_CACHE_CHECK(for multibyte character support,cf_cv_utf8_lib,[
	cf_save_LIBS="$LIBS"
	AC_TRY_LINK([
#include <stdlib.h>
#include <stdio.h>
#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif
],[putwc(0,0);],
	[cf_cv_utf8_lib=yes],
	[CF_FIND_LINKAGE([
#include <libutf8.h>],[putwc(0,0);],utf8,
		[cf_cv_utf8_lib=add-on],
		[cf_cv_utf8_lib=no])
])])

# HAVE_LIBUTF8_H is used by ncurses if curses.h is shared between
# ncurses/ncursesw:
if test "$cf_cv_utf8_lib" = "add-on" ; then
	AC_DEFINE(HAVE_LIBUTF8_H,1,[Define to 1 if we should include libutf8.h])
	CF_ADD_INCDIR($cf_cv_header_path_utf8)
	CF_ADD_LIBDIR($cf_cv_library_path_utf8)
	CF_ADD_LIBS($cf_cv_library_file_utf8)
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_VERBOSE version: 3 updated: 2007/07/29 09:55:12
dnl ----------
dnl Use AC_VERBOSE w/o the warnings
AC_DEFUN([CF_VERBOSE],
[test -n "$verbose" && echo "	$1" 1>&AC_FD_MSG
CF_MSG_LOG([$1])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WCHAR_TYPE version: 4 updated: 2012/10/06 16:39:58
dnl -------------
dnl Check if type wide-character type $1 is declared, and if so, which header
dnl file is needed.  The second parameter is used to set a shell variable when
dnl the type is not found.  The first parameter sets a shell variable for the
dnl opposite sense.
AC_DEFUN([CF_WCHAR_TYPE],
[
# This is needed on Tru64 5.0 to declare $1
AC_CACHE_CHECK(if we must include wchar.h to declare $1,cf_cv_$1,[
AC_TRY_COMPILE([
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#ifdef HAVE_LIBUTF8_H
#include <libutf8.h>
#endif],
	[$1 state],
	[cf_cv_$1=no],
	[AC_TRY_COMPILE([
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <wchar.h>
#ifdef HAVE_LIBUTF8_H
#include <libutf8.h>
#endif],
	[$1 value],
	[cf_cv_$1=yes],
	[cf_cv_$1=unknown])])])

if test "$cf_cv_$1" = yes ; then
	AC_DEFINE(NEED_WCHAR_H,1,[Define to 1 if we must include wchar.h])
	NEED_WCHAR_H=1
fi

ifelse([$2],,,[
# if we do not find $1 in either place, use substitution to provide a fallback.
if test "$cf_cv_$1" = unknown ; then
	$2=1
fi
])
ifelse($3,,,[
# if we find $1 in either place, use substitution to provide a fallback.
if test "$cf_cv_$1" != unknown ; then
	$3=1
fi
])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_CURSES_DIR version: 4 updated: 2021/01/02 19:22:58
dnl ------------------
dnl Wrapper for AC_ARG_WITH to specify directory under which to look for curses
dnl libraries.
AC_DEFUN([CF_WITH_CURSES_DIR],[

AC_MSG_CHECKING(for specific curses-directory)
AC_ARG_WITH(curses-dir,
	[  --with-curses-dir=DIR   directory in which (n)curses is installed],
	[cf_cv_curses_dir=$withval],
	[cf_cv_curses_dir=no])
AC_MSG_RESULT($cf_cv_curses_dir)

if test -n "$cf_cv_curses_dir" && test "$cf_cv_curses_dir" != "no"
then
	CF_PATH_SYNTAX(withval)
	if test -d "$cf_cv_curses_dir"
	then
		CF_ADD_INCDIR($cf_cv_curses_dir/include)
		CF_ADD_LIBDIR($cf_cv_curses_dir/lib)
	fi
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_DBMALLOC version: 7 updated: 2010/06/21 17:26:47
dnl ----------------
dnl Configure-option for dbmalloc.  The optional parameter is used to override
dnl the updating of $LIBS, e.g., to avoid conflict with subsequent tests.
AC_DEFUN([CF_WITH_DBMALLOC],[
CF_NO_LEAKS_OPTION(dbmalloc,
	[  --with-dbmalloc         test: use Conor Cahill's dbmalloc library],
	[USE_DBMALLOC])

if test "$with_dbmalloc" = yes ; then
	AC_CHECK_HEADER(dbmalloc.h,
		[AC_CHECK_LIB(dbmalloc,[debug_malloc]ifelse([$1],,[],[,$1]))])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_DMALLOC version: 7 updated: 2010/06/21 17:26:47
dnl ---------------
dnl Configure-option for dmalloc.  The optional parameter is used to override
dnl the updating of $LIBS, e.g., to avoid conflict with subsequent tests.
AC_DEFUN([CF_WITH_DMALLOC],[
CF_NO_LEAKS_OPTION(dmalloc,
	[  --with-dmalloc          test: use Gray Watson's dmalloc library],
	[USE_DMALLOC])

if test "$with_dmalloc" = yes ; then
	AC_CHECK_HEADER(dmalloc.h,
		[AC_CHECK_LIB(dmalloc,[dmalloc_debug]ifelse([$1],,[],[,$1]))])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_LIB_BASENAME version: 1 updated: 2020/03/07 20:05:14
dnl --------------------
dnl Allow for overriding the basename of a library, i.e., the part to which
dnl prefixes/suffixes are attached.
dnl
dnl $1 = variable to set
dnl $2 = option name
dnl $3 = default basename for library, if omitted use $2
AC_DEFUN([CF_WITH_LIB_BASENAME],
[
AC_MSG_CHECKING(for desired basename for $2 library)
AC_ARG_WITH($2-libname,
	[  --with-$2-libname=XXX override ifelse($3,,$2,$3) basename of library],
	[with_lib_basename=$withval],
	[with_lib_basename=ifelse($3,,$2,$3)])
$1="$with_lib_basename"

case "x[$]$1" in
(x|xno|xnone|xyes)
	$1=ifelse($3,,$2,$3)
	;;
(*)
	;;
esac

AC_MSG_RESULT([$]$1)
AC_SUBST($1)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_NCURSES_ETC version: 5 updated: 2016/02/20 19:23:20
dnl -------------------
dnl Use this macro for programs which use any variant of "curses", e.g.,
dnl "ncurses", and "PDCurses".  Programs that can use curses and some unrelated
dnl library (such as slang) should use a "--with-screen=XXX" option.
dnl
dnl This does not use AC_DEFUN, because that would tell autoconf to run each
dnl of the macros inside this one - before this macro.
define([CF_WITH_NCURSES_ETC],[
CF_WITH_CURSES_DIR

cf_cv_screen=curses

AC_MSG_CHECKING(for specified curses library type)
AC_ARG_WITH(screen,
	[  --with-screen=XXX       use specified curses-libraries],
	[cf_cv_screen=$withval],[

AC_ARG_WITH(ncursesw,
	[  --with-ncursesw         use wide ncurses-libraries],
	[cf_cv_screen=ncursesw],[

AC_ARG_WITH(ncurses,
	[  --with-ncurses          use ncurses-libraries],
	[cf_cv_screen=ncurses],[

AC_ARG_WITH(pdcurses,
	[  --with-pdcurses         compile/link with pdcurses X11 library],
	[cf_cv_screen=pdcurses],[

AC_ARG_WITH(curses-colr,
	[  --with-curses-colr      compile/link with HPUX 10.x color-curses],
	[cf_cv_screen=curses_colr],[

AC_ARG_WITH(curses-5lib,
	[  --with-curses-5lib      compile/link with SunOS 5lib curses],
	[cf_cv_screen=curses_5lib])])])])])])

AC_MSG_RESULT($cf_cv_screen)

case $cf_cv_screen in
(curses|curses_*)
	CF_CURSES_CONFIG
	;;
(ncursesw*)
	CF_UTF8_LIB
	CF_NCURSES_CONFIG($cf_cv_screen)
	;;
(ncurses*)
	CF_NCURSES_CONFIG($cf_cv_screen)
	;;
(pdcurses)
	CF_PDCURSES_X11
	;;
(*)
	AC_MSG_ERROR(unexpected screen-value: $cf_cv_screen)
	;;
esac

CF_NCURSES_PTHREADS($cf_cv_screen)

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_SCREEN_PDCURSES version: 1 updated: 2020/08/28 16:56:27
dnl -----------------------
dnl Call this macro before CF_ENABLE_WARNINGS for configure scripts which use
dnl the "--with-screen=pdcurses" selection.  Doing that allows the configure
dnl script to search for the X11/Xt header files to declare (or not) the
dnl symbol needed to enable "const" in those header files.  If that configure
dnl option is not used, then those checks are unnecessary.
AC_DEFUN([CF_WITH_SCREEN_PDCURSES],[
AC_PROVIDE([AC_PATH_XTRA])
AC_PROVIDE([AC_PATH_X])
if test -n "$with_screen" && test "x$with_screen" = "xpdcurses"
then
	AC_PATH_X
	AC_PATH_XTRA
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_VALGRIND version: 1 updated: 2006/12/14 18:00:21
dnl ----------------
AC_DEFUN([CF_WITH_VALGRIND],[
CF_NO_LEAKS_OPTION(valgrind,
	[  --with-valgrind         test: use valgrind],
	[USE_VALGRIND])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_WITH_X11_RGB version: 2 updated: 2019/12/31 08:53:54
dnl ---------------
dnl Handle configure option "--with-x11-rgb", setting these shell
dnl variables:
dnl
dnl $RGB_PATH is the option value, used for finding the X11 rgb file.
dnl $no_x11_rgb is a "#" (comment) if "--without-x11-rgb" is given.
dnl
dnl Most Linux's use this:
dnl 	/usr/share/X11/rgb.txt
dnl Debian uses this:
dnl 	/etc/X11/rgb.txt
dnl DragonFlyBSD ports uses this:
dnl 	/usr/pkg/lib/X11/rgb.txt
dnl FreeBSD ports use these:
dnl 	/usr/local/lib/X11/rgb.txt
dnl 	/usr/local/share/X11/rgb.txt
dnl Mandriva has these:
dnl 	/usr/lib/X11/rgb.txt
dnl 	/usr/lib64/X11/rgb.txt
dnl NetBSD has these
dnl 	/usr/X11R7/lib/X11/rgb.txt
dnl OpenSolaris uses
dnl 	32-bit:
dnl 	/usr/X11/etc/X11/rgb.txt
dnl 	/usr/X11/share/X11/rgb.txt
dnl 	/usr/X11/lib/X11/rgb.txt
dnl OSX uses
dnl		/opt/local/share/X11/rgb.txt (MacPorts)
dnl		/opt/X11/share/X11/rgb.txt (non-ports)
dnl	64-bit:
dnl 	/usr/X11/etc/X11/rgb.txt
dnl 	/usr/X11/share/X11/rgb.txt (perhaps)
dnl 	/usr/X11/lib/amd64/X11/rgb.txt
dnl Solaris10 uses (in this order):
dnl 	/usr/openwin/lib/X11/rgb.txt
dnl 	/usr/X11/lib/X11/rgb.txt
AC_DEFUN([CF_WITH_X11_RGB],[
AC_MSG_CHECKING(for X11 rgb file)
AC_ARG_WITH(x11-rgb,
	[  --with-x11-rgb=FILE   file containing X11 rgb information (EPREFIX/lib/X11/rgb.txt)],
	[RGB_PATH=$withval],
	[RGB_PATH=auto])

if test "x[$]RGB_PATH" = xauto
then
	RGB_PATH='${exec_prefix}/lib/X11/rgb.txt'
	for cf_path in \
		/opt/local/share/X11/rgb.txt \
		/opt/X11/share/X11/rgb.txt \
		/usr/share/X11/rgb.txt \
		/usr/X11/share/X11/rgb.txt \
		/usr/X11/lib/X11/rgb.txt \
		/usr/lib/X11/rgb.txt \
		/etc/X11/rgb.txt \
		/usr/pkg/lib/X11/rgb.txt \
		/usr/X11R7/lib/X11/rgb.txt \
		/usr/X11R6/lib/X11/rgb.txt \
		/usr/X11R5/lib/X11/rgb.txt \
		/usr/X11R4/lib/X11/rgb.txt \
		/usr/local/lib/X11/rgb.txt \
		/usr/local/share/X11/rgb.txt \
		/usr/lib64/X11/rgb.txt
	do
		if test -f "$cf_path" ; then
			RGB_PATH="$cf_path"
			break
		fi
	done
else
	cf_path=$RGB_PATH
	CF_PATH_SYNTAX(cf_path)
fi

AC_MSG_RESULT($RGB_PATH)
AC_SUBST(RGB_PATH)
AC_DEFINE_UNQUOTED(RGB_PATH,"$cf_path",[Define to the full pathname of rgb.txt])

no_x11_rgb=
if test "$RGB_PATH" = no
then
	no_x11_rgb="#"
fi
AC_SUBST(no_x11_rgb)
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_XOPEN_CURSES version: 17 updated: 2021/07/10 12:22:27
dnl ---------------
dnl Test if we should define X/Open source for curses, needed on Digital Unix
dnl 4.x, to see the extended functions, but breaks on IRIX 6.x.
dnl
dnl The getbegyx() check is needed for HPUX, which omits legacy macros such
dnl as getbegy().  The latter is better design, but the former is standard.
AC_DEFUN([CF_XOPEN_CURSES],
[
AC_REQUIRE([CF_CURSES_CPPFLAGS])dnl
AC_CACHE_CHECK(definition to turn on extended curses functions,cf_cv_need_xopen_extension,[
cf_cv_need_xopen_extension=unknown
AC_TRY_LINK([
#include <stdlib.h>
#include <${cf_cv_ncurses_header:-curses.h}>],[
#if defined(NCURSES_VERSION_PATCH)
#if (NCURSES_VERSION_PATCH < 20100501) && (NCURSES_VERSION_PATCH >= 20100403)
	make an error
#endif
#endif
#ifdef NCURSES_WIDECHAR
make an error	/* prefer to fall-through on the second checks */
#endif
	cchar_t check;
	int check2 = curs_set((int)sizeof(check));
	long x = winnstr(stdscr, "", 0);
	int x1, y1;
	(void)check2;
	getbegyx(stdscr, y1, x1);
	(void)x;
	(void)y1;
	(void)x1;
	],
	[cf_cv_need_xopen_extension=none],
	[
	for cf_try_xopen_extension in _XOPEN_SOURCE_EXTENDED NCURSES_WIDECHAR
	do
		AC_TRY_LINK([
#define $cf_try_xopen_extension 1
#include <stdlib.h>
#include <${cf_cv_ncurses_header:-curses.h}>],[
		cchar_t check;
		int check2 = curs_set((int)sizeof(check));
		long x = winnstr(stdscr, "", 0);
		int x1, y1;
		getbegyx(stdscr, y1, x1);
		(void)check2;
		(void)x;
		(void)y1;
		(void)x1;
		],
		[cf_cv_need_xopen_extension=$cf_try_xopen_extension; break])
	done
	])
])

case "$cf_cv_need_xopen_extension" in
(*_*)
	CF_APPEND_TEXT(CPPFLAGS,-D$cf_cv_need_xopen_extension)
	;;
esac

])dnl
dnl ---------------------------------------------------------------------------
dnl CF_XOPEN_SOURCE version: 59 updated: 2021/08/28 15:20:37
dnl ---------------
dnl Try to get _XOPEN_SOURCE defined properly that we can use POSIX functions,
dnl or adapt to the vendor's definitions to get equivalent functionality,
dnl without losing the common non-POSIX features.
dnl
dnl Parameters:
dnl	$1 is the nominal value for _XOPEN_SOURCE
dnl	$2 is the nominal value for _POSIX_C_SOURCE
AC_DEFUN([CF_XOPEN_SOURCE],[
AC_REQUIRE([AC_CANONICAL_HOST])
AC_REQUIRE([CF_POSIX_VISIBLE])

if test "$cf_cv_posix_visible" = no; then

cf_XOPEN_SOURCE=ifelse([$1],,500,[$1])
cf_POSIX_C_SOURCE=ifelse([$2],,199506L,[$2])
cf_xopen_source=

case "$host_os" in
(aix[[4-7]]*)
	cf_xopen_source="-D_ALL_SOURCE"
	;;
(msys)
	cf_XOPEN_SOURCE=600
	;;
(darwin[[0-8]].*)
	cf_xopen_source="-D_APPLE_C_SOURCE"
	;;
(darwin*)
	cf_xopen_source="-D_DARWIN_C_SOURCE"
	cf_XOPEN_SOURCE=
	;;
(freebsd*|dragonfly*|midnightbsd*)
	# 5.x headers associate
	#	_XOPEN_SOURCE=600 with _POSIX_C_SOURCE=200112L
	#	_XOPEN_SOURCE=500 with _POSIX_C_SOURCE=199506L
	cf_POSIX_C_SOURCE=200112L
	cf_XOPEN_SOURCE=600
	cf_xopen_source="-D_BSD_TYPES -D__BSD_VISIBLE -D_POSIX_C_SOURCE=$cf_POSIX_C_SOURCE -D_XOPEN_SOURCE=$cf_XOPEN_SOURCE"
	;;
(hpux11*)
	cf_xopen_source="-D_HPUX_SOURCE -D_XOPEN_SOURCE=500"
	;;
(hpux*)
	cf_xopen_source="-D_HPUX_SOURCE"
	;;
(irix[[56]].*)
	cf_xopen_source="-D_SGI_SOURCE"
	cf_XOPEN_SOURCE=
	;;
(linux*|uclinux*|gnu*|mint*|k*bsd*-gnu|cygwin)
	CF_GNU_SOURCE($cf_XOPEN_SOURCE)
	;;
(minix*)
	cf_xopen_source="-D_NETBSD_SOURCE" # POSIX.1-2001 features are ifdef'd with this...
	;;
(mirbsd*)
	# setting _XOPEN_SOURCE or _POSIX_SOURCE breaks <sys/select.h> and other headers which use u_int / u_short types
	cf_XOPEN_SOURCE=
	CF_POSIX_C_SOURCE($cf_POSIX_C_SOURCE)
	;;
(netbsd*)
	cf_xopen_source="-D_NETBSD_SOURCE" # setting _XOPEN_SOURCE breaks IPv6 for lynx on NetBSD 1.6, breaks xterm, is not needed for ncursesw
	;;
(openbsd[[6-9]]*)
	# OpenBSD 6.x has broken locale support, both compile-time and runtime.
	# see https://www.mail-archive.com/bugs@openbsd.org/msg13200.html
	# Abusing the conformance level is a workaround.
	AC_MSG_WARN(this system does not provide usable locale support)
	cf_xopen_source="-D_BSD_SOURCE"
	cf_XOPEN_SOURCE=700
	;;
(openbsd[[4-5]]*)
	# setting _XOPEN_SOURCE lower than 500 breaks g++ compile with wchar.h, needed for ncursesw
	cf_xopen_source="-D_BSD_SOURCE"
	cf_XOPEN_SOURCE=600
	;;
(openbsd*)
	# setting _XOPEN_SOURCE breaks xterm on OpenBSD 2.8, is not needed for ncursesw
	;;
(osf[[45]]*)
	cf_xopen_source="-D_OSF_SOURCE"
	;;
(nto-qnx*)
	cf_xopen_source="-D_QNX_SOURCE"
	;;
(sco*)
	# setting _XOPEN_SOURCE breaks Lynx on SCO Unix / OpenServer
	;;
(solaris2.*)
	cf_xopen_source="-D__EXTENSIONS__"
	cf_cv_xopen_source=broken
	;;
(sysv4.2uw2.*) # Novell/SCO UnixWare 2.x (tested on 2.1.2)
	cf_XOPEN_SOURCE=
	cf_POSIX_C_SOURCE=
	;;
(*)
	CF_TRY_XOPEN_SOURCE
	CF_POSIX_C_SOURCE($cf_POSIX_C_SOURCE)
	;;
esac

if test -n "$cf_xopen_source" ; then
	CF_APPEND_CFLAGS($cf_xopen_source,true)
fi

dnl In anything but the default case, we may have system-specific setting
dnl which is still not guaranteed to provide all of the entrypoints that
dnl _XOPEN_SOURCE would yield.
if test -n "$cf_XOPEN_SOURCE" && test -z "$cf_cv_xopen_source" ; then
	AC_MSG_CHECKING(if _XOPEN_SOURCE really is set)
	AC_TRY_COMPILE([#include <stdlib.h>],[
#ifndef _XOPEN_SOURCE
make an error
#endif],
	[cf_XOPEN_SOURCE_set=yes],
	[cf_XOPEN_SOURCE_set=no])
	AC_MSG_RESULT($cf_XOPEN_SOURCE_set)
	if test "$cf_XOPEN_SOURCE_set" = yes
	then
		AC_TRY_COMPILE([#include <stdlib.h>],[
#if (_XOPEN_SOURCE - 0) < $cf_XOPEN_SOURCE
make an error
#endif],
		[cf_XOPEN_SOURCE_set_ok=yes],
		[cf_XOPEN_SOURCE_set_ok=no])
		if test "$cf_XOPEN_SOURCE_set_ok" = no
		then
			AC_MSG_WARN(_XOPEN_SOURCE is lower than requested)
		fi
	else
		CF_TRY_XOPEN_SOURCE
	fi
fi
fi # cf_cv_posix_visible
])
dnl ---------------------------------------------------------------------------
dnl CF_X_ATHENA version: 24 updated: 2020/03/10 18:53:47
dnl -----------
dnl Check for Xaw (Athena) libraries
dnl
dnl Sets $cf_x_athena according to the flavor of Xaw which is used.
AC_DEFUN([CF_X_ATHENA],
[
cf_x_athena=${cf_x_athena:-Xaw}

AC_MSG_CHECKING(if you want to link with Xaw 3d library)
withval=
AC_ARG_WITH(Xaw3d,
	[  --with-Xaw3d            link with Xaw 3d library])
if test "$withval" = yes ; then
	cf_x_athena=Xaw3d
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING(if you want to link with Xaw 3d xft library)
withval=
AC_ARG_WITH(Xaw3dxft,
	[  --with-Xaw3dxft         link with Xaw 3d xft library])
if test "$withval" = yes ; then
	cf_x_athena=Xaw3dxft
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING(if you want to link with neXT Athena library)
withval=
AC_ARG_WITH(neXtaw,
	[  --with-neXtaw           link with neXT Athena library])
if test "$withval" = yes ; then
	cf_x_athena=neXtaw
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
fi

AC_MSG_CHECKING(if you want to link with Athena-Plus library)
withval=
AC_ARG_WITH(XawPlus,
	[  --with-XawPlus          link with Athena-Plus library])
if test "$withval" = yes ; then
	cf_x_athena=XawPlus
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
fi

cf_x_athena_lib=""

if test "$PKG_CONFIG" != none ; then
	cf_athena_list=
	test "$cf_x_athena" = Xaw && cf_athena_list="xaw8 xaw7 xaw6"
	for cf_athena_pkg in \
		$cf_athena_list \
		${cf_x_athena} \
		${cf_x_athena}-devel \
		lib${cf_x_athena} \
		lib${cf_x_athena}-devel
	do
		CF_TRY_PKG_CONFIG($cf_athena_pkg,[
			cf_x_athena_lib="$cf_pkgconfig_libs"
			CF_UPPER(cf_x_athena_LIBS,HAVE_LIB_$cf_x_athena)
			AC_DEFINE_UNQUOTED($cf_x_athena_LIBS)

			CF_TRIM_X_LIBS

AC_CACHE_CHECK(for usable $cf_x_athena/Xmu package,cf_cv_xaw_compat,[
AC_TRY_LINK([
#include <X11/Xmu/CharSet.h>
],[
int check = XmuCompareISOLatin1("big", "small");
(void)check;
],[cf_cv_xaw_compat=yes],[cf_cv_xaw_compat=no])])

			if test "$cf_cv_xaw_compat" = no
			then
				# workaround for broken ".pc" files...
				case "$cf_x_athena_lib" in
				(*-lXmu*)
					;;
				(*)
					CF_VERBOSE(work around broken package)
					cf_save_xmu="$LIBS"
					cf_first_lib=`echo "$cf_save_xmu" | sed -e 's/^[ ][ ]*//' -e 's/ .*//'`
					CF_TRY_PKG_CONFIG(xmu,[
							LIBS="$cf_save_xmu"
							CF_ADD_LIB_AFTER($cf_first_lib,$cf_pkgconfig_libs)
						],[
							CF_ADD_LIB_AFTER($cf_first_lib,-lXmu)
						])
					CF_TRIM_X_LIBS
					;;
				esac
			fi

			break])
	done
fi

if test -z "$cf_x_athena_lib" ; then
	CF_X_EXT
	CF_X_TOOLKIT
	CF_X_ATHENA_CPPFLAGS($cf_x_athena)
	CF_X_ATHENA_LIBS($cf_x_athena)
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_X_ATHENA_CPPFLAGS version: 9 updated: 2020/12/31 10:54:15
dnl --------------------
dnl Normally invoked by CF_X_ATHENA, with $1 set to the appropriate flavor of
dnl the Athena widgets, e.g., Xaw, Xaw3d, neXtaw.
AC_DEFUN([CF_X_ATHENA_CPPFLAGS],
[
AC_REQUIRE([AC_PATH_XTRA])
cf_x_athena_root=ifelse([$1],,Xaw,[$1])
cf_x_athena_inc=""

for cf_path in default \
	/usr/contrib/X11R6 \
	/usr/contrib/X11R5 \
	/usr/lib/X11R5 \
	/usr/local
do
	if test -z "$cf_x_athena_inc" ; then
		CF_SAVE_XTRA_FLAGS([CF_X_ATHENA_CPPFLAGS])
		cf_test=X11/$cf_x_athena_root/SimpleMenu.h
		if test "$cf_path" != default ; then
			CF_APPEND_TEXT(CPPFLAGS,-I$cf_path/include)
			AC_MSG_CHECKING(for $cf_test in $cf_path)
		else
			AC_MSG_CHECKING(for $cf_test)
		fi
		AC_TRY_COMPILE([
#include <X11/Intrinsic.h>
#include <$cf_test>],[],
			[cf_result=yes],
			[cf_result=no])
		AC_MSG_RESULT($cf_result)
		CF_RESTORE_XTRA_FLAGS([CF_X_ATHENA_CPPFLAGS])
		if test "$cf_result" = yes ; then
			test "$cf_path"  = default && cf_x_athena_inc=default
			test "$cf_path" != default && cf_x_athena_inc="$cf_path/include"
			break
		fi
	fi
done

if test -z "$cf_x_athena_inc" ; then
	AC_MSG_WARN([Unable to find Athena header files])
elif test "$cf_x_athena_inc" != default ; then
	CF_APPEND_TEXT(CPPFLAGS,-I$cf_x_athena_inc)
fi
])
dnl ---------------------------------------------------------------------------
dnl CF_X_ATHENA_LIBS version: 13 updated: 2020/01/11 18:16:10
dnl ----------------
dnl Normally invoked by CF_X_ATHENA, with $1 set to the appropriate flavor of
dnl the Athena widgets, e.g., Xaw, Xaw3d, neXtaw.
AC_DEFUN([CF_X_ATHENA_LIBS],
[AC_REQUIRE([CF_X_TOOLKIT])
cf_x_athena_root=ifelse([$1],,Xaw,[$1])
cf_x_athena_lib=""

for cf_path in default \
	/usr/contrib/X11R6 \
	/usr/contrib/X11R5 \
	/usr/lib/X11R5 \
	/usr/local
do
	for cf_lib in \
		${cf_x_athena_root} \
		${cf_x_athena_root}7 \
		${cf_x_athena_root}6
	do
	for cf_libs in \
		"-l$cf_lib -lXmu" \
		"-l$cf_lib -lXpm -lXmu" \
		"-l${cf_lib}_s -lXmu_s"
	do
		test -n "$cf_x_athena_lib" && break

		CF_SAVE_XTRA_FLAGS([CF_X_ATHENA_LIBS])
		cf_test=XawSimpleMenuAddGlobalActions
		test "$cf_path" != default && cf_libs="-L$cf_path/lib $cf_libs"
		CF_ADD_LIBS($cf_libs)
		AC_MSG_CHECKING(for $cf_test in $cf_libs)
		AC_TRY_LINK([
#include <X11/Intrinsic.h>
#include <X11/$cf_x_athena_root/SimpleMenu.h>
],[
$cf_test((XtAppContext) 0)],
			[cf_result=yes],
			[cf_result=no])
		AC_MSG_RESULT($cf_result)
		CF_RESTORE_XTRA_FLAGS([CF_X_ATHENA_LIBS])

		if test "$cf_result" = yes ; then
			cf_x_athena_lib="$cf_libs"
			break
		fi
	done # cf_libs
		test -n "$cf_x_athena_lib" && break
	done # cf_lib
done

if test -z "$cf_x_athena_lib" ; then
	AC_MSG_ERROR(
[Unable to successfully link Athena library (-l$cf_x_athena_root) with test program])
fi

CF_ADD_LIBS($cf_x_athena_lib)
CF_UPPER(cf_x_athena_LIBS,HAVE_LIB_$cf_x_athena)
AC_DEFINE_UNQUOTED($cf_x_athena_LIBS)
])
dnl ---------------------------------------------------------------------------
dnl CF_X_EXT version: 3 updated: 2010/06/02 05:03:05
dnl --------
AC_DEFUN([CF_X_EXT],[
CF_TRY_PKG_CONFIG(Xext,,[
	AC_CHECK_LIB(Xext,XextCreateExtension,
		[CF_ADD_LIB(Xext)])])
])dnl
dnl ---------------------------------------------------------------------------
dnl CF_X_TOOLKIT version: 26 updated: 2021/01/02 09:31:20
dnl ------------
dnl Check for X Toolkit libraries
AC_DEFUN([CF_X_TOOLKIT],
[
AC_REQUIRE([AC_PATH_XTRA])
AC_REQUIRE([CF_CHECK_CACHE])

# OSX is schizoid about who owns /usr/X11 (old) versus /opt/X11 (new) (and
# in some cases has installed dummy files in the former, other cases replaced
# it with a link to the new location).  This complicates the configure script.
# Check for that pitfall, and recover using pkg-config
#
# If none of these are set, the configuration is almost certainly broken.
if test -z "${X_CFLAGS}${X_PRE_LIBS}${X_LIBS}${X_EXTRA_LIBS}"
then
	CF_TRY_PKG_CONFIG(x11,,[AC_MSG_WARN(unable to find X11 library)])
	CF_TRY_PKG_CONFIG(ice,,[AC_MSG_WARN(unable to find ICE library)])
	CF_TRY_PKG_CONFIG(sm,,[AC_MSG_WARN(unable to find SM library)])
	CF_TRY_PKG_CONFIG(xt,,[AC_MSG_WARN(unable to find Xt library)])
else
	LIBS="$X_PRE_LIBS $LIBS $X_EXTRA_LIBS"
fi

cf_have_X_LIBS=no

CF_TRY_PKG_CONFIG(xt,[

	case "x$LIBS" in
	(*-lX11*)
		;;
	(*)
# we have an "xt" package, but it may omit Xt's dependency on X11
AC_CACHE_CHECK(for usable X dependency,cf_cv_xt_x11_compat,[
AC_TRY_LINK([
#include <X11/Xlib.h>
],[
	int rc1 = XDrawLine((Display*) 0, (Drawable) 0, (GC) 0, 0, 0, 0, 0);
	int rc2 = XClearWindow((Display*) 0, (Window) 0);
	int rc3 = XMoveWindow((Display*) 0, (Window) 0, 0, 0);
	int rc4 = XMoveResizeWindow((Display*)0, (Window)0, 0, 0, 0, 0);
],[cf_cv_xt_x11_compat=yes],[cf_cv_xt_x11_compat=no])])
		if test "$cf_cv_xt_x11_compat" = no
		then
			CF_VERBOSE(work around broken X11 dependency)
			# 2010/11/19 - good enough until a working Xt on Xcb is delivered.
			CF_TRY_PKG_CONFIG(x11,,[CF_ADD_LIB_AFTER(-lXt,-lX11)])
		fi
		;;
	esac

AC_CACHE_CHECK(for usable X Toolkit package,cf_cv_xt_ice_compat,[
AC_TRY_LINK([
#include <X11/Shell.h>
],[int num = IceConnectionNumber(0); (void) num
],[cf_cv_xt_ice_compat=yes],[cf_cv_xt_ice_compat=no])])

	if test "$cf_cv_xt_ice_compat" = no
	then
		# workaround for broken ".pc" files used for X Toolkit.
		case "x$X_PRE_LIBS" in
		(*-lICE*)
			case "x$LIBS" in
			(*-lICE*)
				;;
			(*)
				CF_VERBOSE(work around broken ICE dependency)
				CF_TRY_PKG_CONFIG(ice,
					[CF_TRY_PKG_CONFIG(sm)],
					[CF_ADD_LIB_AFTER(-lXt,$X_PRE_LIBS)])
				;;
			esac
			;;
		esac
	fi

	cf_have_X_LIBS=yes
],[

	LDFLAGS="$X_LIBS $LDFLAGS"
	CF_CHECK_CFLAGS($X_CFLAGS)

	AC_CHECK_FUNC(XOpenDisplay,,[
	AC_CHECK_LIB(X11,XOpenDisplay,
		[CF_ADD_LIB(X11)])])

	AC_CHECK_FUNC(XtAppInitialize,,[
	AC_CHECK_LIB(Xt, XtAppInitialize,
		[AC_DEFINE(HAVE_LIBXT,1,[Define to 1 if we can compile with the Xt library])
		 cf_have_X_LIBS=Xt
		 LIBS="-lXt $LIBS"])])
])

if test "$cf_have_X_LIBS" = no ; then
	AC_MSG_WARN(
[Unable to successfully link X Toolkit library (-lXt) with
test program.  You will have to check and add the proper libraries by hand
to makefile.])
fi
])dnl
dnl ---------------------------------------------------------------------------
dnl CF__CURSES_DATA version: 3 updated: 2021/01/04 19:45:09
dnl ---------------
dnl Attempt to make a copy of a curses data item.  This is needed in the
dnl check-data configure tests when using ncurses, because the symbol may be
dnl actually a function return-value.  That could happen if the linker is
dnl broken (does not resolve data-only references), or if ncurses is configured
dnl to support re-entrant code.
dnl $1 = target
dnl $2 = source
define([CF__CURSES_DATA],[
#if defined($2) && ((defined(NCURSES_WRAPPED_VAR) && (NCURSES_VERSION_PATCH < 20200208)) || defined(NCURSES_BROKEN_LINKER) || defined(NCURSES_REENTRANT))
	const void *$1 = (const void *)($2);
#else
	const void *$1 = &($2);
#endif
	fprintf(stderr, "testing linkage of $2:%p\\n", (const void *)$1);
])dnl
dnl ---------------------------------------------------------------------------
dnl CF__CURSES_HEAD version: 2 updated: 2010/10/23 15:54:49
dnl ---------------
dnl Define a reusable chunk which includes <curses.h> and <term.h> when they
dnl are both available.
define([CF__CURSES_HEAD],[
#ifdef HAVE_XCURSES
#include <xcurses.h>
char * XCursesProgramName = "test";
#else
#include <${cf_cv_ncurses_header:-curses.h}>
#if defined(NCURSES_VERSION) && defined(HAVE_NCURSESW_TERM_H)
#include <ncursesw/term.h>
#elif defined(NCURSES_VERSION) && defined(HAVE_NCURSES_TERM_H)
#include <ncurses/term.h>
#elif defined(HAVE_TERM_H)
#include <term.h>
#endif
#endif
])
