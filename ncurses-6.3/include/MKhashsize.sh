#!/bin/sh
##############################################################################
# Copyright 2019,2020 Thomas E. Dickey                                       #
# Copyright 1998,2006 Free Software Foundation, Inc.                         #
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
# $Id: MKhashsize.sh,v 1.9 2020/02/02 23:34:34 tom Exp $
#
# MKhashsize.sh --- generate size include for hash functions
#
echo "/*"
echo " * hashsize.h -- hash and token table constants"
echo " */"

test $# = 0 && set Caps
TABSIZE=`cat "$@" | grep -v '^[ #]' | grep -v "^$" | grep -v "^capalias"| grep -v "^infoalias" | grep -v "^userdef" | grep -v "^used_by" | wc -l`

echo ""
echo "#define CAPTABSIZE	${TABSIZE}"
echo "#define HASHTABSIZE	(${TABSIZE} * 2)"
