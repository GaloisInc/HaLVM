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
allow for a much wider variety of use cases. When connected with the appopriate
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

We develop the HaLVM almost exclusively in Fedora Linux 16 or 17 running their
associated versions of Xen. Please see the HaLVM wiki at GitHub for information
on how you can tune your version of Xen to be better for debugging.

Please follow the directions there for checking out the most recent release
of the HaLVM. If you plan to do development work on the HaLVM itself, please
fork the HaLVM. This allows us to more easily tell who is working on the HaLVM,
and GitHub's tools make merging your changes much more easy.

Once checked out, the HaLVM utilizes a very traditional build process:

> ./configure
> make

We typically recommend building the HaLVM in place. The above commands will
install the HaLVM into your local directory, in the path `dist/bin/`. This
is how we use the HaLVM internally, and so is the most likely to work. We
also include a less-tested capability to install the HaLVM tools into another
directory:

> ./configure --prefix=...
> make
> make install

However, be warned that this is frequently less tested than the former
installation options.

3. Where To Look Next
---------------------

The HaLVM comes with a number of examples / test cases, located in the folder
`static-bits/examples`. We suggest taking a look at these to see a wide
variety of HaLVM programs you might use as a starting point. In addition,
the HaLVM builds full documentation for all of its libraries, which you can
find wherever you installed the HaLVM

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
