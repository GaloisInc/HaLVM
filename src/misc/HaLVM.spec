%define debug_package %{nil}
%define with_gmp %{?_with_gmp:1}%{?!_with_gmp:0}

%if %with_gmp
%define name_suffix -gmp
%define gmp_flag    --enable-gmp
%else
%define name_suffix %{nil}
%define gmp_flag    %{nil}
%endif

Name:		HaLVM%{name_suffix}
Version:	%{_version}
Release:	%{_release}%{?dist}
Summary:	The Haskell Lightweight Virtual Machine

Group:		System Environment/Base
License:	BSD3
URL:		https://github.com/GaloisInc/HaLVM
Source0:	HaLVM-%{_version}.tar.gz

BuildRequires:	autoconf automake libtool patch gcc ncurses-devel
BuildRequires:  zlib-devel chrpath
BuildRequires:  /usr/include/xen/xen.h
Requires:       perl glibc libffi libgcc ncurses-libs zlib gcc
Requires:       /usr/include/xen/xen.h
AutoReq:        no

%description
The Haskell Lightweight Virtual Machine, or HaLVM, is a port of the
Glasgow Haskell Compiler toolsuite to enable developers to write
high-level, lightweight virtual machines that can run directly on the
Xen hypervisor.

%prep
%setup -q -n HaLVM-%{_version}

%build
# We can't used %%configure because it overwrites halvm-ghc/config.sub for
# no apparant reasons
./configure --prefix=%{_prefix} --exec-prefix=%{_exec_prefix}  --bindir=%{_bindir} --sbindir=%{_sbindir} --sysconfdir=%{_sysconfdir} --datadir=%{_datadir} --includedir=%{_includedir} --libdir=%{_libdir} --libexecdir=%{_libexecdir} --localstatedir=%{_localstatedir} --sharedstatedir=%{_sharedstatedir} --mandir=%{_mandir} --infodir=%{_infodir} %{gmp_flag}
make 

%install
make install DESTDIR=%{buildroot}
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/alex
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/cabal
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/ghc
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/ghc-pkg
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/haddock.bin
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/happy
chrpath -r %{_libdir}/HaLVM-%{version}/lib %{buildroot}%{_libdir}/HaLVM-%{version}/bin/hsc2hs.bin
chrpath -d %{buildroot}%{_libdir}/HaLVM-%{version}/bin/HsColour
rm -rf  %{buildroot}share
mv %{buildroot}%{_docdir}/ghc* %{buildroot}%{_docdir}/ghc
mv %{buildroot}%{_docdir}/ghc %{buildroot}%{_docdir}/HaLVM-ghc
mkdir -p %{buildroot}%{_datadir}/HaLVM-%{version}
cp -r examples %{buildroot}%{_datadir}/HaLVM-%{version}/

%post
%{_bindir}/halvm-ghc-pkg recache

%files
%defattr(-,root,root)
%{_libdir}/HaLVM-%{version}/*
%{_libdir}/libIVC.a
%{_bindir}/convert-profile
%{_bindir}/halvm-cabal
%{_bindir}/halvm-config
%{_bindir}/halvm-ghc
%{_bindir}/halvm-ghc-pkg
%{_bindir}/mkrenddir
%{_bindir}/*-unknown-HaLVM-ghc*
%{_bindir}/*-unknown-HaLVM-hp2ps
%{_includedir}/libIVC.h
%{_docdir}/HaLVM-ghc/html/libraries/array-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/base-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/binary-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/bytestring-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/containers-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/deepseq-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/filepath-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/ghc-boot-th-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/ghc-prim-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/hoopl-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/pretty-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/template-haskell-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/time-*/LICENSE
%{_docdir}/HaLVM-ghc/html/libraries/transformers-*/LICENSE
%{_datadir}/HaLVM-%{version}/*

%if %with_gmp
%{_docdir}/HaLVM-ghc/html/libraries/integer-gmp-*/LICENSE
%else
%{_docdir}/HaLVM-ghc/html/libraries/integer-simple-*/LICENSE
%endif

%changelog
* Thu Jul 02 2015 Adam Wick <awick@galois.com>
- Initial RPM release.
