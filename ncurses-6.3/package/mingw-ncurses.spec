%?mingw_package_header

Summary: shared libraries for terminal handling
Name: mingw32-ncurses6
Version: 6.3
Release: 20211021
License: X11
Group: Development/Libraries
Source: ncurses-%{version}-%{release}.tgz
# URL: https://invisible-island.net/ncurses/

BuildRequires:  mingw32-filesystem >= 95
BuildRequires:  mingw32-gcc
BuildRequires:  mingw32-binutils

BuildRequires:  mingw64-filesystem >= 95
BuildRequires:  mingw64-gcc
BuildRequires:  mingw64-binutils

%global MY_ABI 6

%define CC_NORMAL -Wall -Wstrict-prototypes -Wmissing-prototypes -Wshadow -Wconversion
%define CC_STRICT %{CC_NORMAL} -W -Wbad-function-cast -Wcast-align -Wcast-qual -Wmissing-declarations -Wnested-externs -Wpointer-arith -Wwrite-strings -ansi -pedantic

%description -n mingw32-ncurses6
Cross-compiling support for ncurses to mingw32.

The ncurses library routines are a terminal-independent method of
updating character screens with reasonable optimization.

This package is used for testing ABI 6 with cross-compiles to MinGW.

%package -n mingw64-ncurses6
Summary:        Curses library for MinGW64

%description -n mingw64-ncurses6
Cross-compiling support for ncurses to mingw64.

The ncurses library routines are a terminal-independent method of
updating character screens with reasonable optimization.

This package is used for testing ABI %{MY_ABI} with cross-compiles to MinGW.

%prep

%define CFG_OPTS \\\
	--disable-db-install \\\
	--disable-echo \\\
	--disable-getcap \\\
	--disable-hard-tabs \\\
	--disable-leaks \\\
	--disable-macros \\\
	--disable-overwrite \\\
	--disable-termcap \\\
	--enable-interop \\\
	--enable-opaque-curses \\\
	--enable-opaque-form \\\
	--enable-opaque-menu \\\
	--enable-opaque-panel \\\
	--enable-pc-files \\\
	--enable-sp-funcs \\\
	--enable-term-driver \\\
	--enable-warnings \\\
	--enable-wgetch-events \\\
	--enable-widec \\\
	--with-config-suffix=dev \\\
	--verbose \\\
	--with-cxx-shared \\\
	--with-develop \\\
	--with-fallbacks=unknown,xterm \\\
	--with-tic-path=/usr/bin/tic%{MY_ABI} \\\
	--with-infocmp-path=/usr/bin/infocmp%{MY_ABI} \\\
	--with-install-prefix=$RPM_BUILD_ROOT \\\
	--with-pc-suffix=%{MY_ABI} \\\
	--with-pcre2 \\\
	--with-shared \\\
	--with-tparm-arg=intptr_t \\\
	--with-trace \\\
	--with-xterm-kbs=DEL \\\
	--without-ada \\\
	--without-debug \\\
	--without-manpages \\\
	--without-progs \\\
	--without-tests

%define debug_package %{nil}
%setup -q -n ncurses-%{version}-%{release}

%build
mkdir BUILD-W32
pushd BUILD-W32
CFLAGS="%{CC_NORMAL}" \
CC=%{mingw32_cc} \
%mingw32_configure %{CFG_OPTS} \
	--with-pkg-config-libdir=%{mingw32_libdir}/pkgconfig
make
popd

mkdir BUILD-W64
pushd BUILD-W64
CFLAGS="%{CC_NORMAL}" \
CC=%{mingw64_cc} \
%mingw64_configure %{CFG_OPTS} \
	--with-pkg-config-libdir=%{mingw64_libdir}/pkgconfig
make
popd

%install
rm -rf $RPM_BUILD_ROOT

mkdir -p $RPM_BUILD_ROOT%{_bindir}

pushd BUILD-W32
%{mingw32_make} install.libs
for name in $RPM_BUILD_ROOT%{mingw32_bindir}/*-config; \
	do \
		base=`basename $name`; \
		ln -v $name $RPM_BUILD_ROOT%{_bindir}/%{mingw32_target}-$base; \
	done
popd

pushd BUILD-W64
%{mingw64_make} install.libs
for name in $RPM_BUILD_ROOT%{mingw64_bindir}/*-config; \
	do \
		base=`basename $name`; \
		ln -v $name $RPM_BUILD_ROOT%{_bindir}/%{mingw64_target}-$base; \
	done
popd

%clean
rm -rf $RPM_BUILD_ROOT

%files -n mingw32-ncurses6
%defattr(-,root,root,-)
%{_bindir}/%{mingw32_target}-*
%{mingw32_bindir}/*
%{mingw32_includedir}/*
%{mingw32_libdir}/*

%files -n mingw64-ncurses6
%defattr(-,root,root,-)
%{_bindir}/%{mingw64_target}-*
%{mingw64_bindir}/*
%{mingw64_includedir}/*
%{mingw64_libdir}/*

%changelog

* Sun Jun 30 2019 Thomas E. Dickey
- use tic-path and infocmp-path options for fallbacks

* Sat Feb 10 2018 Thomas E. Dickey
- add several development features

* Tue Dec 26 2017 Thomas E. Dickey
- add --with-config-suffix option

* Sat Sep 20 2014 Thomas E. Dickey
- adjust install-rules for ncurses*-config

* Sat Aug 03 2013 Thomas E. Dickey
- initial version, using mingw-pdcurses package as a guide.
