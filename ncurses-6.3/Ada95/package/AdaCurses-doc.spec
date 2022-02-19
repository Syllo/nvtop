Summary: AdaCurses - Ada95 binding documentation for ncurses
%define AppProgram AdaCurses
%define AppVersion MAJOR.MINOR
%define AppRelease YYYYMMDD
%define AppPackage %{AppProgram}-doc
# $Id: AdaCurses-doc.spec,v 1.7 2019/11/23 21:15:53 tom Exp $
Name: %{AppPackage}
Version: %{AppVersion}
Release: %{AppRelease}
License: MIT
Group: Applications/Development
URL: ftp://ftp.invisible-island.net/%{AppProgram}
Source0: %{AppProgram}-%{AppRelease}.tgz
Packager: Thomas Dickey <dickey@invisible-island.net>

%description
This is the Ada95 binding documentation from the ncurses MAJOR.MINOR
distribution, for patch-date YYYYMMDD.
%prep

%define debug_package %{nil}

%setup -q -n %{AppProgram}-%{AppRelease}

%build

INSTALL_PROGRAM='${INSTALL}' \
%configure \
	--target %{_target_platform} \
	--prefix=%{_prefix} \
	--datadir=%{_datadir} \
	--with-ada-sharedlib

%install
[ "$RPM_BUILD_ROOT" != "/" ] && rm -rf $RPM_BUILD_ROOT

(cd doc && make install.html DESTDIR=$RPM_BUILD_ROOT )

%clean
if rm -rf $RPM_BUILD_ROOT; then
  echo OK
else
  find $RPM_BUILD_ROOT -type f | grep -F -v /.nfs && exit 1
fi
exit 0

%files
%defattr(-,root,root)
%{_datadir}/doc/%{AppProgram}

%changelog
# each patch should add its ChangeLog entries here

* Sat Nov 16 2019 Thomas Dickey
- modify clean-rule to work around Fedora NFS bugs.

* Sat Sep 07 2019 Thomas Dickey
- use AppProgram to replace "AdaCurses" globally

* Sat Mar 26 2011 Thomas Dickey
- initial version
