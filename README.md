# HaLVM v3

This is a branch containing version 3.0 of the Haskell Lightweight Virtual
Machine, or HaLVM. HaLVM v3.0 is a complete rethink of the HaLVM as a concept,
with the following key goals:

  1. Limit the number of changes required to GHC in order to host it on
     bare metal.
        * Currently, the only modification required to GHC is one
          which disables unsafe foreign function calls.
  1. Be able to automatically retarget HaLVM executables to more platforms,
     including KVM and VMware, as well as more-easily-debugged platforms
     like Linux.
  1. Support most UNIX-capable Haskell programs out of the box.

# Current Status

Currently, nothing works and nothing builds.

Development is mostly occurring in the `HALVMCore` and `HaLVMInterfaces`
directories, as we try to tie the C substrate back into our Haskell
interfaces.

# Current Design

In a normal system, a Haskell program links against a version of `libc`, which
(internally) calls a bunch of operating system system calls. For the HaLVM, we
want to continue having Haskell programs link against a version of libc, as per
normal. In fact, we want to use the GHC POSIX backends as much as possible, with
as few changes as possible.

The version of libc we're using is called `musl`, and is hosted in the
subdirectory of the same name. `Musl` is a fairly compatible `libc` that is
likely to support a wide range of libraries that people might want to link into
their Haskell programs. However, while it is platform-agnostic, it is
Linux-specific.

To deal with this, we do some madness. We define our own 'HaLVM' architecture
for libc, in which instead of defining all the syscalls as assembly stubs, we
define them as linker symbols composed of the string "halvm_syscall_" concatenated
with the name of the syscall. This is ... somewhat bad form, but doesn't seem to
break anything.

These symbols will then be resolved to implementations in the `HALVMCore`
library, which then variously:

   1. Just returns `ENOSYS`, `EINVAL`, or some other appropriate error
      for system calls that should obviously fail on unikernels. (`swapon`,
      for example, or `execve`.)
   1. Provides a HaLVM-specific implementation, which may further call out
      into host-specific implementations. For example, many threading-related
      calls are in this category, as are core memory primitives.
   1. Provides an indirection point, in which higher-level systems can
      inject their own implementations of critical functions. The network
      stack, file system, and console are examples. This mechanism heavily
      uses GHC's Backpack functionality, and the library `HaLVMInterfaces`.
