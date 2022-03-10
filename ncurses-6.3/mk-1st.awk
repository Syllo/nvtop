# $Id: mk-1st.awk,v 1.124 2021/10/15 00:50:05 tom Exp $
##############################################################################
# Copyright 2018-2020,2021 Thomas E. Dickey                                  #
# Copyright 1998-2016,2017 Free Software Foundation, Inc.                    #
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
# Author: Thomas E. Dickey
#
# Generate list of objects for a given model library
# Variables:
#	name		  (library name, e.g., "ncurses", "panel", "forms", "menus")
#	traces		  ("all" or "DEBUG", to control whether tracing is compiled in)
#	MODEL		  (e.g., "DEBUG", uppercase; toupper is not portable)
#	CXX_MODEL	  (e.g., "DEBUG", uppercase)
#	LIB_SUFFIX	  (e.g., "", "w", "t", "tw")
#	USE_LIB_SUFFIX (e.g., "", "w", "t", "tw")
#	model		  (directory into which we compile, e.g., "obj")
#	prefix		  (e.g., "lib", for Unix-style libraries)
#	suffix		  (e.g., "_g.a", for debug libraries)
#	subset		  ("none", "base", "base+ext_funcs" or "termlib", etc.)
#	driver		  ("yes" or "no", depends on --enable-term-driver)
#	ShlibVer	  ("rel", "abi" or "auto", to augment DoLinks variable)
#	ShlibVerInfix ("yes" or "no", determines location of version #)
#	SymLink		  ("ln -s", etc)
#	TermlibRoot	  ("tinfo" or other root for libterm.so)
#	TermlibSuffix (".so" or other suffix for libterm.so)
#	ReLink		  ("yes", or "no", flag to rebuild shared libs on install)
#	ReRanlib	  ("yes", or "no", flag to rerun ranlib for installing static)
#	DoLinks		  ("yes", "reverse" or "no", flag to add symbolic links)
#	rmSoLocs	  ("yes" or "no", flag to add extra clean target)
#	ldconfig	  (path for this tool, if used)
#	make_phony    ("yes" if the make-program accepts ".PHONY" directive.
#	overwrite	  ("yes" or "no", flag to add link to libcurses.a
#	depend		  (optional dependencies for all objects, e.g, ncurses_cfg.h)
#	host		  (cross-compile host, if any)
#	libtool_version (libtool "-version-info" or "-version-number")
#
# Notes:
#	CLIXs nawk does not like underscores in command-line variable names.
#	Mixed-case variable names are ok.
#	HP-UX requires shared libraries to have executable permissions.
#
function is_ticlib() {
		return ( subset ~ /^ticlib$/ );
	}
function is_termlib() {
		return ( subset ~ /^(ticlib\+)?termlib((\+[^+ ]+)*\+[a-z_]+_tinfo)?$/ );
	}
# see lib_name
function lib_name_of(a_name) {
		return sprintf("%s%s%s", prefix, a_name, suffix)
	}
# see imp_name
function imp_name_of(a_name) {
		if (ShlibVerInfix == "cygdll" || ShlibVerInfix == "msysdll" || ShlibVerInfix == "mingw") {
			result = sprintf("%s%s%s.a", prefix, a_name, suffix);
		} else if (ShlibVerInfix == "msvcdll") {
			result = sprintf("%s%s%s.lib", prefix, a_name, suffix);
		} else{
			result = "";
		}
		return result;
	}
# see abi_name
function abi_name_of(a_name) {
		if (ShlibVerInfix == "cygdll") {
			result = sprintf("%s%s$(ABI_VERSION)%s", "cyg", a_name, suffix);
		} else if (ShlibVerInfix == "msysdll") {
			result = sprintf("%s%s$(ABI_VERSION)%s", "msys-", a_name, suffix);
		} else if (ShlibVerInfix == "mingw" || ShlibVerInfix == "msvcdll") {
			result = sprintf("%s%s$(ABI_VERSION)%s", prefix, a_name, suffix);
		} else if (ShlibVerInfix == "yes") {
			result = sprintf("%s%s.$(ABI_VERSION)%s", prefix, a_name, suffix);
		} else {
			result = sprintf("%s.$(ABI_VERSION)", lib_name_of(a_name));
		}
		return result;
	}
# see rel_name
function rel_name_of(a_name) {
		if (ShlibVerInfix == "cygdll") {
			result = sprintf("%s%s$(REL_VERSION)%s", "cyg", a_name, suffix);
		} else if (ShlibVerInfix == "msysdll") {
			result = sprintf("%s%s$(ABI_VERSION)%s", "msys-", a_name, suffix);
		} else if (ShlibVerInfix == "mingw" || ShlibVerInfix == "msvcdll") {
			result = sprintf("%s%s$(REL_VERSION)%s", prefix, a_name, suffix);
		} else if (ShlibVerInfix == "yes") {
			result = sprintf("%s%s.$(REL_VERSION)%s", prefix, a_name, suffix);
		} else {
			result = sprintf("%s.$(REL_VERSION)", lib_name_of(a_name));
		}
		return result;
	}
# see end_name
function end_name_of(a_name) {
		if ( MODEL != "SHARED" ) {
			result = lib_name_of(a_name);
		} else if ( DoLinks == "reverse") {
			result = lib_name_of(a_name);
		} else {
			if ( ShlibVer == "rel" ) {
				result = rel_name_of(a_name);
			} else if ( ShlibVer == "abi" || ShlibVer == "cygdll" || ShlibVer == "msysdll" || ShlibVer == "mingw" || ShlibVer == "msvcdll" ) {
				result = abi_name_of(a_name);
			} else {
				result = lib_name_of(a_name);
			}
		}
		return result
	}
function symlink(src,dst) {
		if ( src != dst ) {
			if ( SymLink !~ /.*-f.*/ ) {
				printf "rm -f %s; ", dst
			}
			printf "$(LN_S) %s %s; ", src, dst
		}
	}
function rmlink(directory, dst) {
		if ( dst != "" ) {
			printf "\t-rm -f %s/%s\n", directory, dst
		}
	}
function removelinks(directory) {
		nlinks = 0;
		links[nlinks++] = end_name;
		if ( DoLinks == "reverse" ) {
			if ( ShlibVer == "rel" ) {
				links[nlinks++] = abi_name;
				links[nlinks++] = rel_name;
			} else if ( ShlibVer == "abi" ) {
				links[nlinks++] = abi_name;
			}
		} else {
			if ( ShlibVer == "rel" ) {
				links[nlinks++] = abi_name;
				links[nlinks++] = lib_name;
			} else if ( ShlibVer == "abi" ) {
				links[nlinks++] = lib_name;
			}
		}
		for (j = 0; j < nlinks; ++j) {
			found = 0;
			for (k = 0; k < j; ++k ) {
				if ( links[j] == links[k] ) {
					found = 1;
					break;
				}
			}
			if ( !found ) {
				rmlink(directory, links[j]);
			}
		}
	}
function make_shlib(objs, shlib_list) {
		printf "\t$(MK_SHARED_LIB) $(%s_OBJS) $(%s)\n", objs, shlib_list
	}
function sharedlinks(directory) {
		if ( ShlibVer != "auto" && ShlibVer != "cygdll" && ShlibVer != "msysdll" && ShlibVer != "mingw" && ShlibVer != "msvcdll" ) {
			printf "\tcd %s && (", directory
			if ( DoLinks == "reverse" ) {
				if ( ShlibVer == "rel" ) {
					symlink(lib_name, abi_name);
					symlink(abi_name, rel_name);
				} else if ( ShlibVer == "abi" ) {
					symlink(lib_name, abi_name);
				}
			} else {
				if ( ShlibVer == "rel" ) {
					symlink(rel_name, abi_name);
					symlink(abi_name, lib_name);
				} else if ( ShlibVer == "abi" ) {
					symlink(abi_name, lib_name);
				}
			}
			printf ")\n"
		}
	}
# termlib may be named explicitly via "--with-termlib=XXX", which overrides
# any suffix.  Temporarily override "suffix" to account for this.
function termlib_end_of() {
	termlib_save_suffix = suffix;
	suffix = TermlibSuffix;
	termlib_temp_result = end_name_of(TermlibRoot);
	suffix = termlib_save_suffix;
	return termlib_temp_result;
}
function shlib_build(directory) {
		dst_libs = sprintf("%s/%s", directory, end_name);
		printf "%s : \\\n", dst_libs
		if (subset == "ticlib" && driver == "yes" ) {
			base = name;
			sub(/^tic/, "ncurses", base); # workaround for "w"
			printf "\t\t%s/%s \\\n", directory, end_name_of(base);
		}
		if (subset ~ /^base/ || subset == "ticlib" ) {
			save_suffix = suffix
			sub(/^[^.]\./,".",suffix)
			if (directory != "../lib") {
				printf "\t\t%s/%s \\\n", "../lib", termlib_end_of();
			}
			printf "\t\t%s/%s \\\n", directory, termlib_end_of();
			suffix = save_suffix
		}
		printf "\t\t$(RESULTING_SYMS) $(%s_OBJS)\n", OBJS
		printf "\t@echo linking $@\n"
		printf "\t@mkdir -p %s\n", directory
		if ( ReLink != "yes" ) {
			printf "\t@sleep 1\n"
		}
		if ( is_ticlib() ) {
			make_shlib(OBJS, "TICS_LIST")
		} else if ( is_termlib() ) {
			make_shlib(OBJS, "TINFO_LIST")
		} else {
			make_shlib(OBJS, "SHLIB_LIST")
		}
		sharedlinks(directory)
	}
function shlib_install(directory) {
		src_lib1 = sprintf("../lib/%s", end_name);
		dst_lib1 = sprintf("%s/%s", directory, end_name);
		printf "%s : \\\n", dst_lib1
		printf "\t\t%s \\\n", directory
		printf "\t\t%s\n", src_lib1
		printf "\t@echo installing $@\n"
		printf "\t$(INSTALL_LIB) %s %s\n", src_lib1, dst_lib1;
		sharedlinks(directory)
	}
function install_dll(directory,filename) {
		src_name = sprintf("../lib/%s", filename);
		dst_name = sprintf("$(DESTDIR)%s/%s", directory, filename);
		printf "\t@echo installing %s as %s\n", src_name, dst_name
		if ( directory == "$(bindir)" ) {
			program = "$(INSTALL) -m 755";
		} else {
			program = "$(INSTALL_LIB)";
		}
		printf "\t%s %s %s\n", program, src_name, dst_name
	}
function in_subset(value) {
		value = " " value " ";
		check = subset;
		gsub("[+]", " ", check);
		check = " " check " ";
		return index(check,value);
	}
function trim_suffix(value) {
	if (USE_LIB_SUFFIX != "" && length(value) > length(USE_LIB_SUFFIX)) {
		check = substr(value, 1 + length(value) - length(USE_LIB_SUFFIX));
		if (check == USE_LIB_SUFFIX) {
			value = substr(value, 1, length(value) - length(USE_LIB_SUFFIX));
		}
	}
	return value;
}
BEGIN	{
		TOOL_PREFIX = "";
		found = 0;
		using = 0;
	}
	/^@/ {
		using = 0
		if (subset == "none") {
			using = 1
			print  ""
			print "# generated by mk-1st.awk"
		} else if (in_subset($2) > 0) {
			if (using == 0) {
				if (found == 0) {
					if ( name ~ /^.*\+\+.*/ ) {
						if ( CXX_MODEL == "NORMAL" && MODEL == "SHARED" ) {
							print  ""
							printf "# overriding model from %s to match CXX_MODEL\n", MODEL;
							MODEL = "NORMAL";
							suffix = ".a";
							DoLinks = "no";
						}
					}
					print  ""
					printf "# generated by mk-1st.awk (subset=%s)\n", subset
					printf "#  name:            %s\n", name
					printf "#  traces:          %s\n", traces
					printf "#  MODEL:           %s\n", MODEL
					printf "#  CXX_MODEL:       %s\n", CXX_MODEL
					printf "#  LIB_SUFFIX:      %s\n", LIB_SUFFIX
					printf "#  USE_LIB_SUFFIX:  %s\n", USE_LIB_SUFFIX
					printf "#  model:           %s\n", model
					printf "#  prefix:          %s\n", prefix
					printf "#  suffix:          %s\n", suffix
					printf "#  subset:          %s\n", subset
					printf "#  driver:          %s\n", driver
					printf "#  ShlibVer:        %s\n", ShlibVer
					printf "#  ShlibVerInfix:   %s\n", ShlibVerInfix
					printf "#  SymLink:         %s\n", SymLink
					printf "#  TermlibRoot:     %s\n", TermlibRoot
					printf "#  TermlibSuffix:   %s\n", TermlibSuffix
					printf "#  ReLink:          %s\n", ReLink
					printf "#  ReRanlib:        %s\n", ReRanlib
					printf "#  DoLinks:         %s\n", DoLinks
					printf "#  rmSoLocs:        %s\n", rmSoLocs
					printf "#  ldconfig:        %s\n", ldconfig
					printf "#  make_phony:      %s\n", make_phony
					printf "#  overwrite:       %s\n", overwrite
					printf "#  depend:          %s\n", depend
					printf "#  host:            %s\n", host
					printf "#  libtool_version: %s\n", libtool_version
					print  ""
				}
				using = 1
			}
			if ( is_ticlib() ) {
				OBJS  = MODEL "_P"
			} else if ( is_termlib() ) {
				OBJS  = MODEL "_T"
			} else {
				OBJS  = MODEL
			}
		}
	}
	/^[@#]/ {
		next
	}
	$1 ~ /trace/ {
		if (traces != "all" && traces != MODEL && $1 != "lib_trace")
			next
	}
	{
		if (using \
		 && ( $1 != "link_test" ) \
		 && ( $2 == "lib" \
		   || $2 == "progs" \
		   || $2 == "c++" \
		   || $2 == "tack" ))
		{
			if ( found == 0 )
			{
				printf "%s_OBJS =", OBJS
				if ( $2 == "lib" ) {
					found = 1;
				} else if ( $2 == "c++" ) {
					TOOL_PREFIX = "CXX_";
					found = 1;
				} else {
					found = 2;
				}
				if ( $2 == "c++" ) {
					CC_NAME="CXX"
					CC_FLAG="CXXFLAGS"
				} else {
					CC_NAME="CC"
					CC_FLAG="CFLAGS"
				}
			}
			printf " \\\n\t../%s/%s$o", model, $1;
		}
	}
END	{
		print  ""
		if ( found != 0 )
		{
			printf "\n$(%s_OBJS) : %s\n", OBJS, depend
		}
		if ( found == 1 )
		{
			print  ""
			lib_name = lib_name_of(name);
			if ( MODEL == "SHARED" )
			{
				abi_name = abi_name_of(name);
				rel_name = rel_name_of(name);
				imp_name = imp_name_of(name);
				end_name = end_name_of(name);

				shlib_build("../lib")

				print  ""
				print  "install \\"
				print  "install.libs \\"

				if ( ShlibVer == "cygdll" || ShlibVer == "msysdll" || ShlibVer == "mingw" || ShlibVer == "msvcdll") {

					dst_dirs = "$(DESTDIR)$(bindir) $(DESTDIR)$(libdir)";
					printf "install.%s :: %s $(LIBRARIES)\n", name, dst_dirs
					install_dll("$(bindir)",end_name);
					install_dll("$(libdir)",imp_name);

				} else {

					lib_dir = "$(DESTDIR)$(libdir)";
					printf "install.%s :: %s/%s\n", name, lib_dir, end_name
					print ""
					if ( ReLink == "yes" ) {
						shlib_build(lib_dir)
					} else {
						shlib_install(lib_dir)
					}
				}

				if ( overwrite == "yes" && name == "ncurses" )
				{
					if ( ShlibVer == "cygdll" || ShlibVer == "msysdll" || ShlibVer == "mingw" || SlibVer == "msvcdll") {
						if (ShlibVer == "msvcdll") {
							curses_prefix = ""
						} else {
							curses_prefix = "lib"
						}
						ovr_name = sprintf("%scurses%s.a", curses_prefix, suffix)
						printf "\t@echo linking %s to %s\n", imp_name, ovr_name
						printf "\tcd $(DESTDIR)$(libdir) && ("
						symlink(imp_name, ovr_name)
						printf ")\n"
					} else {
						ovr_name = sprintf("libcurses%s", suffix)
						printf "\t@echo linking %s to %s\n", end_name, ovr_name
						printf "\tcd $(DESTDIR)$(libdir) && ("
						symlink(end_name, ovr_name)
						printf ")\n"
					}
				}
				if ( ldconfig != "" && ldconfig != ":" ) {
					printf "\t- test -z \"$(DESTDIR)\" && %s\n", ldconfig
				}
				print  ""
				print  "uninstall \\"
				print  "uninstall.libs \\"
				printf "uninstall.%s ::\n", name
				if ( ShlibVer == "cygdll" || ShlibVer == "msysdll" || ShlibVer == "mingw" || ShlibVer == "msvcdll") {

					printf "\t@echo uninstalling $(DESTDIR)$(bindir)/%s\n", end_name
					printf "\t-@rm -f $(DESTDIR)$(bindir)/%s\n", end_name

					printf "\t@echo uninstalling $(DESTDIR)$(libdir)/%s\n", imp_name
					printf "\t-@rm -f $(DESTDIR)$(libdir)/%s\n", imp_name

				} else {
					printf "\t@echo uninstalling $(DESTDIR)$(libdir)/%s\n", end_name
					removelinks("$(DESTDIR)$(libdir)")
					if ( overwrite == "yes" && name == "ncurses" )
					{
						ovr_name = sprintf("libcurses%s", suffix)
						printf "\t-@rm -f $(DESTDIR)$(libdir)/%s\n", ovr_name
					}
				}
				if ( rmSoLocs == "yes" ) {
					print  ""
					print  "mostlyclean \\"
					print  "clean ::"
					printf "\t-@rm -f so_locations\n"
				}
			}
			else if ( MODEL == "LIBTOOL" )
			{
				end_name = lib_name;
				use_name = trim_suffix(TermlibRoot) USE_LIB_SUFFIX
				printf "../lib/%s : \\\n", lib_name
				if ( (name != use_name ) && ( index(name, "++") == 0 ) && ( index(name, "tic") == 1 || index(name, "ncurses") == 1 ) ) {
					printf "\t\t../lib/lib%s.la \\\n", use_name;
					if ( index(name, "tic") == 1 && index(TermlibRoot, "ncurses") != 1 ) {
						printf "\t\t../lib/lib%s%s.la \\\n", "ncurses", USE_LIB_SUFFIX;
					}
				}
				printf "\t\t$(%s_OBJS)\n", OBJS
				if ( is_ticlib() ) {
					which_list = "TICS_LIST";
				} else if ( is_termlib() ) {
					which_list = "TINFO_LIST";
				} else {
					which_list = "SHLIB_LIST";
				}
				printf "\tcd ../lib && $(LIBTOOL_LINK) $(%s) $(%s) \\\n", CC_NAME, CC_FLAG;
				printf "\t\t-o %s $(%s_OBJS:$o=.lo) \\\n", lib_name, OBJS;
				printf "\t\t-rpath $(libdir) \\\n";
				printf "\t\t%s $(NCURSES_MAJOR):$(NCURSES_MINOR) $(LT_UNDEF) $(%s) $(LDFLAGS)\n", libtool_version, which_list;
				if ( make_phony == "yes" ) {
					print  ""
					printf ".PHONY :\tinstall.%s\n", trim_suffix(name);
				}
				print  ""
				print  "install \\"
				print  "install.libs \\"
				printf "install.%s :: \\\n", trim_suffix(name);
				printf "\t\t$(DESTDIR)$(libdir) \\\n";
				use_name = TermlibRoot USE_LIB_SUFFIX
				if ( (name != use_name ) && ( index(name, "++") == 0 ) && ( index(name, "tic") == 1 || index(name, "ncurses") == 1 ) ) {
					if ( trim_suffix(TermlibRoot) != trim_suffix(name) ) {
						printf "\t\tinstall.%s \\\n", trim_suffix(TermlibRoot);
					}
					if ( index(name, "tic") == 1 && index(TermlibRoot, "ncurses") != 1 && trim_suffix(name) != "ncurses" ) {
						printf "\t\tinstall.%s \\\n", "ncurses";
					}
				}
				printf "\t\t../lib/%s\n", lib_name
				printf "\t@echo installing ../lib/%s as $(DESTDIR)$(libdir)/%s\n", lib_name, lib_name
				printf "\tcd ../lib; $(LIBTOOL_INSTALL) $(INSTALL) %s $(DESTDIR)$(libdir)\n", lib_name
				print  ""
				print  "uninstall \\"
				print  "uninstall.libs \\"
				printf "uninstall.%s ::\n", trim_suffix(name)
				printf "\t@echo uninstalling $(DESTDIR)$(libdir)/%s\n", lib_name
				printf "\t-@$(LIBTOOL_UNINSTALL) rm -f $(DESTDIR)$(libdir)/%s\n", lib_name
			}
			else
			{
				end_name = lib_name;
				printf "../lib/%s : $(%s_OBJS)\n", lib_name, OBJS
				# workaround: binutils' ranlib tries to be clever with
				# timestamps, by pretending its update took no time, confusing
				# the make utility.
				if ( ReLink != "yes" ) {
					printf "\t@sleep 1\n"
				}
				printf "\t$(%sAR) $(%sARFLAGS) $@ $?\n", TOOL_PREFIX, TOOL_PREFIX;
				printf "\t$(RANLIB) $@\n"
				if ( host == "vxworks" )
				{
					printf "\t$(LD) $(LD_OPTS) $? -o $(@:.a=$o)\n"
				}
				print  ""
				print  "install \\"
				print  "install.libs \\"
				printf "install.%s :: $(DESTDIR)$(libdir) ../lib/%s\n", name, lib_name
				printf "\t@echo installing ../lib/%s as $(DESTDIR)$(libdir)/%s\n", lib_name, lib_name
				printf "\t$(INSTALL_DATA) ../lib/%s $(DESTDIR)$(libdir)/%s\n", lib_name, lib_name
				if ( overwrite == "yes" && lib_name == "libncurses.a" )
				{
					printf "\t@echo linking libcurses.a to libncurses.a\n"
					printf "\t-@rm -f $(DESTDIR)$(libdir)/libcurses.a\n"
					printf "\t(cd $(DESTDIR)$(libdir) && "
					symlink("libncurses.a", "libcurses.a")
					printf ")\n"
				}
				if ( ReRanlib == "yes" )
				{
					printf "\t$(RANLIB) $(DESTDIR)$(libdir)/%s\n", lib_name
				}
				if ( host == "vxworks" )
				{
					printf "\t@echo installing ../lib/lib%s$o as $(DESTDIR)$(libdir)/lib%s$o\n", name, name
					printf "\t$(INSTALL_DATA) ../lib/lib%s$o $(DESTDIR)$(libdir)/lib%s$o\n", name, name
				}
				print  ""
				print  "uninstall \\"
				print  "uninstall.libs \\"
				printf "uninstall.%s ::\n", name
				printf "\t@echo uninstalling $(DESTDIR)$(libdir)/%s\n", lib_name
				printf "\t-@rm -f $(DESTDIR)$(libdir)/%s\n", lib_name
				if ( overwrite == "yes" && lib_name == "libncurses.a" )
				{
					printf "\t@echo linking libcurses.a to libncurses.a\n"
					printf "\t-@rm -f $(DESTDIR)$(libdir)/libcurses.a\n"
				}
				if ( host == "vxworks" )
				{
					printf "\t@echo uninstalling $(DESTDIR)$(libdir)/lib%s$o\n", name
					printf "\t-@rm -f $(DESTDIR)$(libdir)/lib%s$o\n", name
				}
			}
			print ""
			print "clean ::"
			removelinks("../lib");
			print ""
			print "mostlyclean::"
			printf "\t-rm -f $(%s_OBJS)\n", OBJS
			if ( MODEL == "LIBTOOL" ) {
				printf "\t-$(LIBTOOL_CLEAN) rm -f $(%s_OBJS:$o=.lo)\n", OBJS
			}
		}
		else if ( found == 2 )
		{
			print ""
			print "mostlyclean::"
			printf "\t-rm -f $(%s_OBJS)\n", OBJS
			if ( MODEL == "LIBTOOL" ) {
				printf "\t-$(LIBTOOL_CLEAN) rm -f $(%s_OBJS:$o=.lo)\n", OBJS
			}
			print ""
			print "clean ::"
			printf "\t-rm -f $(%s_OBJS)\n", OBJS
			if ( MODEL == "LIBTOOL" ) {
				printf "\t-$(LIBTOOL_CLEAN) rm -f $(%s_OBJS:$o=.lo)\n", OBJS
			}
		}
	}
# vile:ts=4 sw=4
