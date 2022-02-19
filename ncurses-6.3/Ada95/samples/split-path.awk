##############################################################################
# Copyright 2020 Thomas E. Dickey                                            #
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
#  Author:  Thomas E. Dickey
#
#  $Id: split-path.awk,v 1.3 2020/02/02 23:34:34 tom Exp $
BEGIN	{ cols = 72; }
/[$]THIS_DATADIR/ {
	gsub("[$]THIS_DATADIR", "\"" this_datadir "\"");
	if ( length ($0) > cols ) {
		indent = index($0, "\"") - 1;
		broken = index($0, "&");
		if (broken > 0 && broken < indent)
			indent = broken - 1;
		leader = sprintf ("%*s& ", indent, " ");
		buffer = $0;
		first  = 1;
		while ( length(buffer) > cols ) {
			printf("%.*s\"\n", cols, buffer);
			buffer = leader "\"" substr(buffer, cols + 1);
		}
		if ( buffer != "" && buffer != leader "\"\"" )
			printf("%.*s\n", cols, buffer);
		next;
	}
}
{
	print;
}
