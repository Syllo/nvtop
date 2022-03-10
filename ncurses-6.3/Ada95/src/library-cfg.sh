#!/bin/sh
##############################################################################
# Copyright 2018-2019,2020 Thomas E. Dickey                                  #
# Copyright 2016,2018 Free Software Foundation, Inc.                         #
#                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a    #
# copy of this software and associated documentation files (the "Software"), #
# to deal in the Software without restriction, including without limitation  #
# the rights to use, copy, modify, merge, publish, distribute, distribute    #
# with modifications, sublicense, and/or sell copies of the Software, and to #
# permit persons to whom the Software is furnished to do so, subject to the  #
# following conditions:                                                      #
#                                                                            #
# The above copyright notice and this permission notice shall be included in #
# all copies or substantial portions of the Software.                        #
#                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    #
# THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER      #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        #
# DEALINGS IN THE SOFTWARE.                                                  #
#                                                                            #
# Except as contained in this notice, the name(s) of the above copyright     #
# holders shall not be used in advertising or otherwise to promote the sale, #
# use or other dealings in this Software without prior written               #
# authorization.                                                             #
##############################################################################
#
# $Id: library-cfg.sh,v 1.7 2020/02/02 23:34:34 tom Exp $
#
# Work around incompatible behavior introduced with gnat6, which causes
# gnatmake to attempt to compile all of the C objects which might be part of
# the project.  This can only work if we provide the compiler flags (done here
# by making a copy of the project file with that information filled in).
model=$1
shift 1
input=$1
shift 1
param=
while test $# != 0
do
	case "x$1" in
	*-[OgDIWf]*)
		test -n "$param" && param="$param,"
		param="$param\"$1\""
		;;
	*)
		echo "${0##*/}: ignored option $1" >&2
		;;
	esac
	shift 1
done

SHARE="-- "
test "x$model" = "xdynamic" && SHARE=

SCRIPT=library-cfg.tmp
cat >$SCRIPT <<EOF
/for Library_Options use /{
	s,-- ,$SHARE,
}
/for Default_Switches ("C") use/{
	s,-- ,,
	s% use .*% use($param);%
}
EOF

sed -f $SCRIPT $input
rc=$?
rm -f $SCRIPT
exit $?
