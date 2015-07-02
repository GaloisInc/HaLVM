The Haskell Lightweight Virtual Machine (HALVM) Source Archive
==============================================================

> The HaLVM is (C) 2008 Galois, Inc., and distributed under a standard,
> three-clause BSD license. Please see the file LICENSE, distributed with
> this software, for specific terms and conditions.

1. What is the HaLVM?
---------------------

The Haskell Lightweight Virtual Machine, or HaLVM, is a port of the Glasgow
Haskell Compiler toolsuite to enable developers to write high-level, lightweight
virtual machines that can run directly on the Xen hypervisor.

While Galois initially designed the HaLVM to allow for quick and easy
prototyping of operating systems components, the HaLVM has grown over time to
allow for a much wider variety of use cases. When connected with the appropriate
libraries, the HaLVM can, for example, operate as a network appliance.

Writing for the Haskell Lightweight Virtual Machine is just like writing
normal Haskell, and many pure Haskell libraries port to the HaLVM with little
or no difficulty. In fact, we include the standard
[Haskell Cabal toolset](http://www.haskell.org/cabal/) in order to more easily
facilitate the integration of outside Haskell libraries. However, instead of
running on top of a typical operating system, HaLVM programs run at a very low
level, directly on the Xen hypervisor. This allows for very lightweight, single
purpose Xen domains with minimal resource requirements.

2. Getting and Building the HaLVM
---------------------------------

The HaLVM is available publicly [on GitHub](http://github.org/GaloisInc/HaLVM).

We develop the HaLVM almost exclusively in Fedora Linux, running a slightly
modified version of their versions of Xen. Our modified versions simply add
the flag "verbose=y" to their build specification, on the lines that build
and install the hypervisor (search for dist-xen and install-xen). We try to
keep the latest version we're using available, in src/misc/xen.spec. In
addition, I will try to keep recent binary and source RPMs available at:

  http://uhsure.com/halvm-xen-rpms/

In addition, you might want to check out Darrin Eden's build system,
available on GitHub:

  https://github.com/dje/halvm-test

It may provide some information that you'd find useful, or may automate portions
of this process for you.

If you plan to do development work on the HaLVM itself, please fork the HaLVM.
This allows us to more easily tell who is working on the HaLVM, and GitHub's
tools make merging your changes much more easy.

You'll need these dependencies:

> autoconf gcc automake libtool patch ncurses-devel xen-devel zlib-devel

If GHC 7.8.4 is not present, a version of it will be downloaded for
use during the build process.

Once checked out, the HaLVM builds as follows:

> git submodule update --init --recursive

> autoconf

> ./configure

> make

> make install

> halvm-ghc-pkg recache

The configure system will accept and honor the "--prefix" flag as per
normal. We also strongly suggest using the "--enable-gmp" flag, in order
to enable the (much faster) GMP library for large integer math.

If you intend on using inter-domain communication (specified in
`Communication.Rendezvous` and `Communication.IVC`), run the `mkrenddir` script
(located wherever you installed the HaLVM, `/usr/local/bin` by default) as root. 
This creates a top-level `/rendezvous` directory in the XenStore with
appropriate permissions.


3. Where To Look Next
---------------------

The HaLVM comes with a number of examples / test cases, located in the folder
`examples`. We suggest taking a look at these to see a wide variety of HaLVM
programs you might use as a starting point.

Further information about using the HaLVM is available on the HaLVM wiki.
Developers interested in improving the HaLVM should also take a look at the
current list of HaLVM bugs, and submit any new feature requests to the
HaLVM bug system.

4. How To Contact Us
--------------------

If you have any questions or suggestions for the HaLVM, please feel free to
join the [HaLVM mailing list](http://community.galois.com/mailman/listinfo/halvm-devel)
and send them to us. Please note that in order to stop a great deal of spam,
you must be a member of the mailing list in order to send messages to it.

If you have bugs you would like to file or patches you'd like to send, we'd
strongly prefer you utilize GitHub's interfaces for both these functions. Please
see GitHub for more information.

That's all, and enjoy using the HaLVM!
