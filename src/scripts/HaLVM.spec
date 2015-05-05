Summary:            The Haskell Lightweight Virtual Machine
Name:               HaLVM
Version:            master
Release:            21%{?dist}
License:            BSD3
Group:              System Environment/Base
URL:                https://github.com/GaloisInc/HaLVM
BuildRequires:      autoconf automake libtool patch gcc ghc
BuildRequires:      ncurses-devel xen-devel zlib-devel cabal-install
BuildRequires:      chrpath

# Avoid "no build ID note" error
%global debug_package %{nil}

%description
The Haskell Lightweight Virtual Machine, or HaLVM, is a port of the Glasgow
Haskell Compiler toolsuite to enable developers to write high-level, lightweight
virtual machines that can run directly on the Xen hypervisor.

%build
rm -rf HaLVM
git clone https://github.com/GaloisInc/HaLVM
cd HaLVM
git submodule update --init --recursive
autoconf
./configure --enable-gmp
make 

%install
make install DESTDIR=%{buildroot}
# Strip rpath from all binaries (they are all static anyway)
chrpath -d %{buildroot}/usr/local/lib/HaLVM-2.0.0/bin/*

%post
/usr/local/bin/halvm-ghc-pkg recache

%files
/usr/local/bin/*
/usr/local/lib/*
/usr/local/include/*
/usr/local/share/*

%changelog

