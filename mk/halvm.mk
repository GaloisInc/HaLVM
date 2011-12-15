# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

include mk/common.mk

################################################################################
# Handy Name-related Macros
################################################################################

package-name = $(HALVM_LIBDIR)/$1-$2/libHS$1-$2.a
rts-header-path = $(TOPDIR)/xen-ghc/rts/$1
halvm-rts-header-path = $(HALVM_LIBDIR)/include/rts/$1


################################################################################
# Extra GHC Headers
################################################################################

HALVM_EXTRA_HEADERS =

define rts-header
# $1 - header path, relative to $(TOPDIR)/xen-ghc/rts
$(call halvm-rts-header-path,$1): $(call rts-header-path,$1)
	$(CP) $$< $$@
ifneq "$(dir $1)" "./"
$(call halvm-rts-header-path,$1): $(call halvm-rts-header-path,$(dir $1))
endif

HALVM_EXTRA_HEADERS += $(call halvm-rts-header-path,$1)
endef

define halvm-rts-header-dir
# $1 - the directory relative to $(HALVM_LIBDIR)/include/rts to create
$(call halvm-rts-header-path,$1):
	$(MKDIR) -p $$@
endef

$(eval $(call halvm-rts-header-dir,sm))
$(eval $(call rts-header,sm/GC.h))



################################################################################
# Halvm
################################################################################

# Everything that needs to be in the bin/ directory of dist/ when we're
# done.
HALVM_BINDIR_REQS   = $(HALVM_GHC)         $(HALVM_GHC_PKG)           \
                      $(HALVM_CABAL)       $(HALVM_CONFIG)            \
                      $(HALVM_MKDIR)       #$(GHC_CABAL)
HALVM_LIBDIR_REQS   = $(HALVM_START_O)     $(HALVM_GHC_BIN)           \
                      $(HALVM_ALEX)        $(HALVM_HADDOCK)           \
                      $(HALVM_HSCOLOUR)    $(HALVM_HSC2HS)            \
                      $(HALVM_GHC_PKG_BIN) $(HALVM_GHC_SPLIT)         \
                      $(HALVM_UNLIT)       $(HALVM_LIBDIR)/libHSffi.a \
                      $(HALVM_INCLUDE_DIR) $(HALVM_GHC_ASM)           \
                      $(HALVM_HAPPY)                                  \
                      $(HALVM_EXTRA_HEADERS)                          \
                      $(HALVM_LIBDIR)/libHSrtsmain.a                  \
                      $(HALVM_LIBDIR)/libHSrts.a                      \
                      $(HALVM_LIBDIR)/package.conf.d/builtin_rts.conf \
                      $(HALVM_LDKERNEL)                               \
                      $(HALVM_LIBDIR)/libm.a                          \
                      $(BASE_PACKAGES)                                \
                      $(HALVM_PACKAGES)
BASE_PACKAGES       = $(call package-name,ghc-prim,0.2.0.0)           \
                      $(call package-name,base,4.2.0.2)               \
                      $(call package-name,array,0.3.0.1)              \
                      $(call package-name,bytestring,0.9.1.7)         \
                      $(call package-name,containers,0.3.0.0)         \
                      $(call package-name,pretty,1.0.1.1)             \
                      $(call package-name,syb,0.1.0.2)                \
                      $(call package-name,extensible-exceptions,0.1.1.1) \
                      $(call package-name,template-haskell,2.4.0.1)   \
                      $(call package-name,time,1.1.4)                 \
                      $(call package-name,old-locale,1.0.0.2)         \
                      $(call package-name,old-time,1.0.0.5)           \
                      $(call package-name,random,1.0.0.2)             \
                      $(call package-name,mtl,1.1.0.2)                \
                      $(call package-name,haskell98,1.0.1.1)
HALVM_PACKAGES      = $(call package-name,BoundedChan,1.0.0.0)        \
                      $(call package-name,HALVMCore,1.0.0)            \
                      $(call package-name,BitFiddler,1.0.0)           \
                      $(call package-name,communication,1.0.0)        \
                      $(call package-name,XenDevice,1.0.0)            \
                      $(call package-name,RendezvousLib,1.0.0)        \
                      $(call package-name,RealDevice,1.0.0)           \
                      $(call package-name,log4h,0.1)
HALVM_SHAREDIR_REQS =

ifeq ($(USE_GMP),true)
  integer           = $(call package-name,integer-gmp,0.2.0.1)
  INTEGER_REQS      = $(HALVM_LIBDIR)/libgmp.a
else
  integer           = $(call package-name,integer-simple,0.1.0.0)
  INTEGER_REQS      =
endif

BASE_PACKAGES    += $(integer)

# Kind of fake targets
HALVM_XENGHC_INCLUDE_TARGET	= $(HALVM_LIBDIR)/include/xenghc/hbmxen.h
DIST_DIR_TREE=$(TOPDIR)/dist/README
HALVM_INCLUDE_DIR=$(HALVM_LIBDIR)/include/ghcconfig.h

# Useful flags
RTS_FLAGS=-optc-fno-delete-null-pointer-checks -optc-mno-red-zone -optc-nostdinc -optc-Dxen_HOST_OS -optc-DRtsWay=\"rts_v\" -optc-Wall -optc-Wextra -optc-Wstrict-prototypes -optc-Wmissing-prototypes -optc-Wmissing-declarations -optc-Winline -optc-Waggregate-return -optc-Wpointer-arith -optc-Wmissing-noreturn -optc-Wnested-externs -optc-Wredundant-decls -optc-I$(TOPDIR)/xen-ghc/rts -optc-I$(HALVM_LIBDIR)/include -optc-DCOMPILING_RTS -optc-fno-strict-aliasing -optc-fno-common -optc-I$(HALVM_LIBDIR)/include/ffi -optc-fomit-frame-pointer -O -package-name rts -static -dcmm-lint -optc-O2 -optc-DProjectVersion=\"$(GHC_VER)\" -optc-DHostPlatform=\"$(ARCH)-unknown-xen\" -optc-DHostArch=\"$(ARCH)\" -optc-DHostOS=\"xen\" -optc-DHostVendor=\"unknown\" -optc-DBuildPlatform=\"$(ARCH)-unknown-linux\" -optc-DBuildArch=\"$(ARCH)\" -optc-DBuildOS=\"linux\" -optc-DBuildVendor=\"unknown\" -optc-DTargetPlatform=\"$(ARCH)-unknown-xen\" -optc-DTargetArch=\"$(ARCH)\" -optc-DTargetOS=\"xen\" -optc-DTargetVendor=\"unknown\" -optc-DGhcUnregisterised=\"NO\" -optc-DGhcEnableTablesNextToCode=\"YES\" -I$(TOPDIR)/xen-ghc/rts/xen/include -I$(TOPDIR)/xen-ghc/rts/xen/include/sys -I$(TOPDIR)/xen-ghc/includes/rts $(GHC_ARCH_OPT) $(GHC_ARCH_FLAGS)

CONF_FLAGS=-Dxen_HOST_OS -I$(TOPDIR)/xen-ghc/includes -I$(HALVM_LIBDIR)/include -DTOP=$(TOPDIR)/dist -DINSTALLING -DLIB_DIR=$(HALVM_LIBDIR) -DPAPI_LIB_DIR= -DINCLUDE_DIR=$(HALVM_LIBDIR)/include -DPAPI_INCLUDE_DIR= $(ARCH_OPT) $(ARCH_FLAGS)

define version_copy_exec
$(SED) -e 's!HALVM_VER!$(HaLVM_VERSION)!g'           \
       -e 's!GHC_VER!$(GHC_VER)!g'                   \
       -e 's!HALVM_HSC2HS_BIN!$(HALVM_HSC2HS_BIN)!g' \
       $1 > $2
$(CHMOD) a+rx $2
endef

define bin_copy_exec
$(CP) $1 $2
$(CHMOD) a+rx $2
endef

LIBIVC_REQS = $(HALVM_LIBDIR)/include/libIVC.h \
              $(HALVM_LIBDIR)/libIVC.a


.PHONY: all
all: $(HALVM_BINDIR_REQS) $(INTEGER_REQS) $(HALVM_LIBDIR_REQS) \
     $(HALVM_SHAREDIR_REQS) $(LIBIVC_REQS)

.PHONY: clean
clean:
	$(RM) -rf dist
	$(MAKE) -C $(TOPDIR)/libIVC clean
	$(FIND) xen-ghc/rts -name '*.o' -delete

$(DIST_DIR_TREE):
	$(MKDIR) $(TOPDIR)/dist
	$(MKDIR) $(TOPDIR)/dist/bin
	$(MKDIR) $(TOPDIR)/dist/lib
	$(MKDIR) $(HALVM_LIBDIR)
	$(MKDIR) $(HALVM_LIBDIR)/include
	$(LN) -s /usr/include/xen $(HALVM_LIBDIR)/include/xen
	$(MKDIR) $(HALVM_LIBDIR)/package.conf.d
	$(MKDIR) $(TOPDIR)/dist/share
	$(MKDIR) $(TOPDIR)/dist/share/doc
	$(MKDIR) $(TOPDIR)/dist/share/man
	$(CP) $(TOPDIR)/static-bits/LICENSE $(TOPDIR)/dist/
	$(CP) $(TOPDIR)/static-bits/README  $(TOPDIR)/dist/

# Bindir stuff

$(HALVM_GHC): $(DIST_DIR_TREE)
	$(call version_copy_exec,$(TOPDIR)/static-bits/bin/halvm-ghc,$@)

$(HALVM_GHC_PKG): $(HALVM_GHC_PKG_BIN) $(DIST_DIR_TREE) \
                  $(TOPDIR)/static-bits/lib/Cabal.conf
	$(call version_copy_exec,$(TOPDIR)/static-bits/bin/halvm-ghc-pkg,$@)
	$(HALVM_GHC_PKG) register $(TOPDIR)/static-bits/lib/Cabal.conf
	$(HALVM_GHC_PKG) recache

$(HALVM_CABAL): $(DIST_DIR_TREE) $(HALVM_CABAL_EXE)
	$(call version_copy_exec,$(TOPDIR)/static-bits/bin/halvm-cabal,$@)

$(HALVM_CABAL_EXE): $(STANDARD_TARGETS) $(HALVM_LIBDIR)/cabal.conf
	$(CP) $(PLATFORM_CABAL_EXE) $(HALVM_CABAL_EXE)

$(HALVM_LIBDIR)/cabal.conf: $(DIST_DIR_TREE)
	$(CPP) -std=c89							\
	       -DCABAL_CACHE_DIR=$(HALVM_LIBDIR)/.cabal			\
	       -DBUILD_DOCS=False					\
	       -DGHC_COMPILER=$(HALVM_GHC)				\
	       -DGHC_PKG=$(HALVM_GHC_PKG)				\
	       -DINSTALL_PREFIX=$(TOPDIR)/dist				\
	       -DHALVM_VER=$(HaLVM_VERSION)				\
	       -DEXTRA_INCDIRS=$(HALVM_LIBDIR)/include			\
	       -DSPLIT_BOJS						\
	       -DINCLUDE_AUX_PROGRAMS					\
	       -DHADDOCK_PROG=$(HALVM_HADDOCK)				\
	       -DHADDOCK_OPTIONS=--optghc=-Dxen_HOST_OS			\
	       -DHSC2HS_PROG=$(HALVM_HSC2HS)				\
	       -DALEX_PROG=$(HALVM_ALEX)				\
		   -DGHC_OPTIONS=							\
	       -DHAPPY_PROG=$(HALVM_HAPPY)				\
	       -DHSCOLOUR_PROG=$(HALVM_HSCOLOUR)			\
	       -DSPLIT_OBJS=True					\
            static-bits/lib/cabal.conf					\
	  | $(SED) "/^#.*/d"						\
	  > $@

$(HALVM_CONFIG): $(DIST_DIR_TREE)
	$(call version_copy_exec,$(TOPDIR)/static-bits/bin/halvm-config,$@)
	$(SED) -i 's|@HALVM_LIBRARY_DIRECTORY@|$(HALVM_LIBDIR)|' $@

$(HALVM_MKDIR): $(DIST_DIR_TREE)
	$(MKDIR) -p dist
	$(MKDIR) -p dist/bin
	$(call version_copy_exec,$(TOPDIR)/static-bits/bin/make_halvm_dir.py,$@)

# compiler stuff

primop-file=$(TOPDIR)/xen-ghc/compiler/primop-$1.hs-incl
define build-primop-rule
$(call primop-file,$1): $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt       \
                        $(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode
	$(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode --$2 \
	   < $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt      \
	   > $(TOPDIR)/xen-ghc/compiler/primop-$1.hs-incl
endef

$(eval $(call build-primop-rule,data-decl,data-decl))
$(eval $(call build-primop-rule,tag,primop-tag))
$(eval $(call build-primop-rule,list,primop-list))
$(eval $(call build-primop-rule,strictness,strictness))
$(eval $(call build-primop-rule,primop-info,primop-primop-info))
$(eval $(call build-primop-rule,out-of-line,out-of-line))
$(eval $(call build-primop-rule,can-fail,can-fail))
$(eval $(call build-primop-rule,has-side-effects,has-side-effects))
$(eval $(call build-primop-rule,needs-wrapper,needs-wrapper))
$(eval $(call build-primop-rule,commutable,commutable))

ALL_HS_INCLS = $(call primop-file,data-decl)   $(call primop-file,tag)         \
               $(call primop-file,list)        $(call primop-file,strictness)  \
               $(call primop-file,primop-info) $(call primop-file,out-of-line) \
               $(call primop-file,can-fail)                                    \
               $(call primop-file,has-side-effects)                            \
               $(call primop-file,needs-wrapper)                               \
               $(call primop-file,commutable)


$(HALVM_GHC_BIN): $(DIST_DIR_TREE) $(HALVM_LIBDIR)/extra-gcc-opts            \
                  $(TOPDIR)/xen-ghc/compiler/main/Config.hs                  \
                  $(ALL_HS_INCLS)
	$(MKDIR) -p xen-ghc/compiler/stage2
	$(CP) static-bits/lib/ghc_boot_platform.$(ARCH).h $(TOPDIR)/xen-ghc/compiler/ghc_boot_platform.h
	( cd xen-ghc/compiler && $(PLATFORM_CABAL) install -fncg -fstage2 -fghci -fbase4 --with-hsc2hs=$(PLATFORM_HSC2HS) --extra-include-dirs=$(TOPDIR)/xen-ghc/compiler/stage2 --ghc-option=-DSTAGE=2)
	$(PLATFORM_GHC) -o $@ -package ghc-6.12.3 -XCPP --make -hidir=$(TOPDIR)/xen-ghc/compiler/dist/build -odir=$(TOPDIR)/xen-ghc/compiler/dist/build $(TOPDIR)/xen-ghc/ghc/Main.hs


# libdir stuff

$(TOPDIR)/xen-ghc/compiler/main/Config.hs: \
    $(DIST_DIR_TREE)                       \
    $(TOPDIR)/static-bits/lib/Config.hs
	cat $(TOPDIR)/static-bits/lib/Config.hs              \
	  | sed -e 's|@INTEGER_LIBRARY@|$(INTEGER_LIBRARY)|' \
	  > $@

$(HALVM_LIBDIR)/extra-gcc-opts:         \
    $(DIST_DIR_TREE)                    \
    $(PLATFORM_LIB_PATH)/extra-gcc-opts
	$(CP) $(PLATFORM_LIB_PATH)/extra-gcc-opts $(HALVM_LIBDIR)/

$(HALVM_ALEX): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_ALEX),$@)

$(HALVM_HAPPY): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_HAPPY),$@)

$(HALVM_HADDOCK): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_HADDOCK),$@)

$(HALVM_HSCOLOUR): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_HSCOLOUR),$@)

$(HALVM_HSC2HS): $(DIST_DIR_TREE)
	$(CP) $(PLATFORM_LIB_PATH)/template-hsc.h $(HALVM_LIBDIR)/
	$(call bin_copy_exec,$(PLATFORM_HSC2HS),$@)

$(HALVM_GHC_PKG_BIN): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_LIB_PATH)/ghc-pkg,$@)

$(HALVM_GHC_SPLIT): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_LIB_PATH)/ghc-split,$@)

$(HALVM_UNLIT): $(DIST_DIR_TREE)
	$(call bin_copy_exec,$(PLATFORM_LIB_PATH)/unlit,$@)

$(HALVM_XENGHC_INCLUDE_TARGET): $(DIST_DIR_TREE) $(HALVM_INCLUDE_DIR)
	$(CP) -rL ./xen-ghc/rts/xen/include $(HALVM_LIBDIR)/include/xenghc

$(HALVM_START_O): xen-ghc/start.$(ARCH).S $(HALVM_XENGHC_INCLUDE_TARGET)
	$(CC) $(CFLAGS) -I$(HALVM_LIBDIR)/include/xenghc -c -o $@ \
	    xen-ghc/start.$(ARCH).S

$(TOPDIR)/xen-ghc/includes/mkDerivedConstants: $(DIST_DIR_TREE) \
  $(TOPDIR)/xen-ghc/includes/mkDerivedConstants.c
	$(CC) $(CFLAGS) -I$(TOPDIR)/xen-ghc/includes -I$(TOPDIR)/xen-ghc/rts -I$(PLATFORM_LIB_PATH)/include -o $@ $(TOPDIR)/xen-ghc/includes/mkDerivedConstants.c

$(HALVM_INCLUDE_DIR): $(DIST_DIR_TREE) \
                      $(TOPDIR)/xen-ghc/includes/mkDerivedConstants
	$(CP) -r $(PLATFORM_LIB_PATH)/include/* $(HALVM_LIBDIR)/include/
	$(CP) $(TOPDIR)/xen-ghc/includes/rts/*.h $(HALVM_LIBDIR)/include/rts/
	$(CP) $(TOPDIR)/xen-ghc/includes/stg/*.h $(HALVM_LIBDIR)/include/stg/
	$(CP) $(TOPDIR)/xen-ghc/includes/Rts.h  $(HALVM_LIBDIR)/include/
	$(TOPDIR)/xen-ghc/includes/mkDerivedConstants > \
	  $(HALVM_LIBDIR)/include/DerivedConstants.h
	$(CP) $(HALVM_LIBDIR)/include/DerivedConstants.h \
	      $(PLATFORM_LIB_PATH)/include/

$(HALVM_GHC_ASM): $(DIST_DIR_TREE) $(HALVM_UNLIT)
	$(HALVM_UNLIT) xen-ghc/driver/mangler/ghc-asm.lprl \
	  			   xen-ghc/driver/mangler/ghc-asm.prl
	$(ECHO) '#!$(PERL)' > $@
	$(ECHO) '$$TARGETPLATFORM = "$(ARCH)-unknown-xen";' >> $@
	$(CAT) xen-ghc/driver/mangler/ghc-asm.prl >> $@
	$(CHMOD) a+rx $@

################################################################################
# builtin-rts
################################################################################

RTS_CFILES=Adjustor.c Arena.c Capability.c ClosureFlags.c Disassembler.c       \
           FrontPanel.c Globals.c Hash.c HsFFI.c Inlines.c Interpreter.c       \
           LdvProfile.c Papi.c Printer.c ProfHeap.c Profiling.c                \
           Proftimer.c RaiseAsync.c RetainerProfile.c RetainerSet.c RtsAPI.c   \
           RtsDllMain.c RtsFlags.c RtsMain.c RtsMessages.c RtsStartup.c        \
           RtsUtils.c Sanity.c Schedule.c Sparks.c Stable.c Stats.c StgCRun.c  \
           StgPrimFloat.c STM.c Task.c ThreadLabels.c ThreadPaused.c Threads.c \
           Ticky.c Timer.c Trace.c Weak.c WSDeque.c                            \
           hooks/FlagDefaults.c hooks/MallocFail.c hooks/OnExit.c              \
           hooks/OutOfHeap.c hooks/RtsOpts.c hooks/RtsOptsEnabled.c            \
           hooks/StackOverflow.c                                               \
           parallel/0Hash.c parallel/0Unpack.c parallel/Dist.c                 \
           parallel/Global.c parallel/GranSim.c parallel/HLComms.c             \
           parallel/LLComms.c parallel/Pack.c parallel/Parallel.c              \
           parallel/ParallelDebug.c parallel/ParInit.c parallel/ParTicky.c     \
           parallel/RBH.c                                                      \
           sm/BlockAlloc.c sm/Compact.c sm/Evac.c sm/GCAux.c sm/GC.c           \
           sm/GCUtils.c sm/MarkWeak.c sm/MBlock.c sm/Scav.o sm/Storage.c       \
           sm/Sweep.c                                                          \
           eventlog/EventLog.c                                                 \
           xen/econsole.c xen/events.c xen/gnttab.c xen/hbmxen.c xen/mm.c      \
           xen/time.c xen/traps.c xen/GHCShims_memory.c xen/GHCShims_signals.c \
           xen/GHCShims_threads.c xen/GHCShims_time.c                          \
           xen/libc/errno.c xen/libc/fcntl.c xen/libc/malloc.c xen/libc/mman.c \
           xen/libc/stdio.c xen/libc/stdlib.c xen/libc/string.c                \
           xen/libc/mntent.c xen/libc/pthread.c
RTS_CMMFILES= PrimOps.cmm StgStartup.cmm Exception.cmm Updates.cmm \
              StgMiscClosures.cmm StgStdThunks.cmm Apply.cmm       \
              HeapStackCheck.cmm AutoApply.cmm

RTS_OFILES =$(addprefix $(TOPDIR)/xen-ghc/rts/,$(RTS_CFILES:.c=.o))
RTS_OFILES+=$(addprefix $(TOPDIR)/xen-ghc/rts/,$(RTS_CMMFILES:.cmm=.o))

%.o: %.c
	$(QUIET_CC)$(PLATFORM_GHC) $(RTS_FLAGS) -c -o $@ $<

%.o: %.cmm
	$(QUIET_CMM)$(PLATFORM_GHC) $(RTS_FLAGS) -c -o $@ $<

$(HALVM_LIBDIR)/libHSrtsmain.a: $(DIST_DIR_TREE) $(TOPDIR)/xen-ghc/rts/Main.o
	$(QUIET_AR) $(RM) -f $@ && $(AR) rcs $@ $(TOPDIR)/xen-ghc/rts/Main.o

$(HALVM_LIBDIR)/libHSrts.a: $(DIST_DIR_TREE) $(RTS_OFILES)
	$(QUIET_AR) $(RM) -f $@ && $(AR) rcs $@ $(RTS_OFILES)

$(HALVM_LIBDIR)/package.conf.d/builtin_rts.conf:    \
    $(DIST_DIR_TREE) $(HALVM_LIBDIR)/libHSrts.a     \
    $(HALVM_LIBDIR)/package.conf.d/builtin_ffi.conf \
    $(TOPDIR)/xen-ghc/rts/package.conf.in
	$(CPP) $(CONF_FLAGS) -o $(TOPDIR)/xen-ghc/rts/package.conf \
	                        $(TOPDIR)/xen-ghc/rts/package.conf.in
	$(SED) 's!^#.*!!g' $(TOPDIR)/xen-ghc/rts/package.conf \
	    | $(HALVM_GHC_PKG) update --force -
	$(HALVM_GHC_PKG) recache



################################################################################
# builtin-ffi
################################################################################

$(HALVM_LIBDIR)/package.conf.d/builtin_ffi.conf: \
    $(DIST_DIR_TREE) $(HALVM_LIBDIR)/libHSffi.a  \
    $(TOPDIR)/xen-ghc/libffi/package.conf.in
	$(CPP) $(CONF_FLAGS) -o $(TOPDIR)/xen-ghc/libffi/package.conf $(TOPDIR)/xen-ghc/libffi/package.conf.in
	$(SED) 's!^#.*!!g' $(TOPDIR)/xen-ghc/libffi/package.conf | $(HALVM_GHC_PKG) update --force -
	$(HALVM_GHC_PKG) recache

$(HALVM_LIBDIR)/libHSffi.a: $(DIST_DIR_TREE)
	$(CP) $(PLATFORM_LIB_PATH)/HSffi.o $(HALVM_LIBDIR)/
	$(CP) $(PLATFORM_LIB_PATH)/libHSffi.a $(HALVM_LIBDIR)/
	$(CP) $(PLATFORM_LIB_PATH)/libHSffi-ghc$(GHC_VER).so $(HALVM_LIBDIR)/


################################################################################
# ghc-prim
################################################################################

$(call package-name,ghc-prim,0.2.0.0) :                              \
          $(TOPDIR)/xen-ghc/libraries/ghc-prim/GHC/Prim.hs           \
          $(TOPDIR)/xen-ghc/libraries/ghc-prim/GHC/PrimopWrappers.hs \
          $(HALVM_CABAL)
	( cd $(TOPDIR)/xen-ghc/libraries/ghc-prim                      && \
	  $(HALVM_CABAL) install                                       && \
	  $(HALVM_GHC_PKG) describe ghc-prim > inplace-pkg-config      && \
	  $(SED) -i 's/GHC.Unit/GHC.Unit GHC.Prim/' inplace-pkg-config && \
	  $(HALVM_GHC_PKG) update --force inplace-pkg-config              \
	)

$(TOPDIR)/xen-ghc/compiler/stage2/ghc_boot_platform.h:
	$(MKDIR) $(TOPDIR)/xen-ghc/compiler/stage2
	$(CP) $(TOPDIR)/static-bits/lib/ghc_boot_platform.$(ARCH).h $@

$(TOPDIR)/xen-ghc/compiler/stage1/ghc_boot_platform.h:
	$(MKDIR) $(TOPDIR)/xen-ghc/compiler/stage1
	$(CP) $(TOPDIR)/static-bits/lib/ghc_boot_platform.$(ARCH).h $@

################################################################################
# STG Application
################################################################################

$(TOPDIR)/xen-ghc/utils/genapply/genapply:
	$(PLATFORM_GHC) --make -Dxen_HOST_OS -o $@                  \
	  -i$(TOPDIR)/xen-ghc/utils/genapply                        \
	  $(TOPDIR)/xen-ghc/utils/genapply/GenApply.hs

$(TOPDIR)/xen-ghc/rts/AutoApply.cmm: $(TOPDIR)/xen-ghc/utils/genapply/genapply
	$< > $@


################################################################################
# Primops
################################################################################

GPOC_FILES = Syntax.hs ParserM.hs Lexer.hs Parser.hs Main.hs
GPOC_FILE_PATHS = $(addprefix $(TOPDIR)/xen-ghc/utils/genprimopcode/,$(GPOC_FILES))

$(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode: $(GPOC_FILE_PATHS)
	$(PLATFORM_GHC) --make -o $@ -i$(TOPDIR)/xen-ghc/utils/genprimopcode \
	    $(TOPDIR)/xen-ghc/utils/genprimopcode/Main.hs

$(TOPDIR)/xen-ghc/compiler/prelude/primops.txt: \
    $(TOPDIR)/xen-ghc/compiler/stage1/ghc_boot_platform.h
	$(CPP) -undef -traditional -P -I$(HALVM_LIBDIR)/include \
	    -I$(TOPDIR)/xen-ghc/compiler -x c                   \
	    $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt.pp   \
	    | grep -v '^#pragma GCC'                            \
	    > $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt

$(TOPDIR)/xen-ghc/libraries/ghc-prim/GHC/Prim.hs:       \
    $(DIST_DIR_TREE)                                    \
    $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt      \
    $(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode
	$(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode  \
	    --make-haskell-source                            \
	    < $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt \
	    > $@

$(TOPDIR)/xen-ghc/libraries/ghc-prim/GHC/PrimopWrappers.hs: \
    $(DIST_DIR_TREE)                                        \
    $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt          \
    $(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode
	$(TOPDIR)/xen-ghc/utils/genprimopcode/genprimopcode  \
	    --make-haskell-wrappers                          \
	    < $(TOPDIR)/xen-ghc/compiler/prelude/primops.txt \
	    > $@


################################################################################
# HaLVM Linker Script
################################################################################

$(HALVM_LDKERNEL): $(STANDARD_TARGETS) $(HALVM_START_O)
	if [ "$(USE_GMP)" == "true" ]; then              \
	    $(SED) -e 's|@LIBGMP@|$${LIBDIR}/libgmp.a|g' \
	    static-bits/lib/ldkernel > $@;               \
	else                                             \
	    $(SED) -e 's|@LIBGMP@||g'                    \
	    static-bits/lib/ldkernel > $@;               \
	fi
	$(CHMOD) +x $@
	$(CP) static-bits/lib/kernel-$(ARCH).lds $(HALVM_LIBDIR)/kernel.lds


################################################################################
# libm
################################################################################

$(HALVM_LIBDIR)/libm.a: $(STANDARD_TARGETS)
	$(MAKE) -C libm
	$(CP) libm/libm.a $(HALVM_LIBDIR)/libm.a


################################################################################
# libIVC
################################################################################

$(HALVM_LIBDIR)/include/libIVC.h: $(TOPDIR)/libIVC/include/libIVC.h
	$(CP) $< $@

$(HALVM_LIBDIR)/libIVC.a: $(TOPDIR)/libIVC/libIVC.a
	$(CP) $< $@


LIBIVC_SOURCES = $(shell ls $(TOPDIR)/libIVC/src/*.c)

$(TOPDIR)/libIVC/libIVC.a: $(TOPDIR)/libIVC/include/libIVC.h $(LIBIVC_SOURCES)
	make -C libIVC


################################################################################
# Cabal Utils
################################################################################

cabal-configure=$(HALVM_CABAL) configure $(CABAL_QUIET_GHC) $(CABAL_QUIET)
cabal-build=$(HALVM_CABAL) build $(CABAL_QUIET)
cabal-install-only=$(HALVM_CABAL) install --only $(CABAL_QUIET)
cabal-unpack=$(HALVM_CABAL) unpack $(CABAL_QUIET)
cabal-register=$(HALVM_CABAL) register $(CABAL_QUIET)
cabal-copy=$(HALVM_CABAL) copy $(CABAL_QUIET)


################################################################################
# All sorts of dependencies
################################################################################

# $1 - package name
# $2 - package version
define remove-rule
remove-$1-$2:
	$(QUIET_RM)$(RM) -f $(call package-name,$1,$2)
	$(HALVM_GHC_PKG) unregister --force $1-$2
endef

# $1 - package name
# $2 - package version
define build-library-rule
$(call package-name,$1,$2) :
	$(QUIET_RM)$(RM) -f $$@
	$(QUIET_LIB_MACRO)( cd $(TOPDIR)/xen-ghc/libraries/$1 && \
	    $(cabal-configure)                                && \
	    $(cabal-build)                                    && \
	    $(cabal-install-only))

$(call remove-rule,$1,$2)
endef

# array
$(eval $(call build-library-rule,array,0.3.0.1))
$(call package-name,array,0.3.0.1): $(call package-name,base,4.2.0.2)

# base
$(eval $(call remove-rule,base,4.2.0.2))
$(call package-name,base,4.2.0.2):
	$(QUIET_RM)$(RM) -f $@
	$(QUIET_LIB)( cd $(TOPDIR)/xen-ghc/libraries/base && \
	    $(cabal-configure)                            && \
	    ($(cabal-build) || $(cabal-build))            && \
	    $(cabal-install-only))
$(call package-name,base,4.2.0.2): $(integer)
$(call package-name,base,4.2.0.2): $(call package-name,ghc-prim,0.2.0.0)

# bytestring
$(eval $(call build-library-rule,bytestring,0.9.1.7))
$(call package-name,bytestring,0.9.1.7): $(call package-name,base,4.2.0.2)

# containers
$(eval $(call build-library-rule,containers,0.3.0.0))
$(call package-name,containers,0.3.0.0): \
    $(call package-name,base,4.2.0.2)    \
    $(call package-name,array,0.3.0.1)

# extensible-exceptions
$(eval $(call build-library-rule,extensible-exceptions,0.1.1.1))
$(call package-name,extensible-exceptions,0.1.1.1): \
    $(call package-name,base,4.2.0.2)

# haskell98
$(eval $(call build-library-rule,haskell98,1.0.1.1))
$(call package-name,haskell98,1.0.1.1):     \
    $(call package-name,array,0.3.0.1)      \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,old-locale,1.0.0.2) \
    $(call package-name,old-time,1.0.0.5)   \
    $(call package-name,random,1.0.0.2)

# integer-simple
$(eval $(call build-library-rule,integer-simple,0.1.0.0))
$(call package-name,integer-simple,0.1.0.0): \
    $(call package-name,ghc-prim,0.2.0.0)

# mtl
$(eval $(call build-library-rule,mtl,1.1.0.2))
$(call package-name,mtl,1.1.0.2): $(call package-name,base,4.2.0.2)

# old-locale
$(eval $(call build-library-rule,old-locale,1.0.0.2))
$(call package-name,old-locale,1.0.0.2): $(call package-name,base,4.2.0.2)

# old-time
$(eval $(call build-library-rule,old-time,1.0.0.5))
$(call package-name,old-time,1.0.0.5):      \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,old-locale,1.0.0.2)

# pretty
$(eval $(call build-library-rule,pretty,1.0.1.1))
$(call package-name,pretty,1.0.1.1): $(call package-name,base,4.2.0.2)

# random
$(eval $(call build-library-rule,random,1.0.0.2))
$(call package-name,random,1.0.0.2):  \
    $(call package-name,base,4.2.0.2) \
    $(call package-name,time,1.1.4)

# syb
$(eval $(call build-library-rule,syb,0.1.0.2))
$(call package-name,syb,0.1.0.2): $(call package-name,base,4.2.0.2)

# template-haskell
$(eval $(call build-library-rule,template-haskell,2.4.0.1))
$(call package-name,template-haskell,2.4.0.1): \
    $(call package-name,base,4.2.0.2)          \
    $(call package-name,containers,0.3.0.0)    \
    $(call package-name,pretty,1.0.1.1)

# time
$(eval $(call build-library-rule,time,1.1.4))
$(call package-name,time,1.1.4):            \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,old-locale,1.0.0.2)


################################################################################
# Hackage Packages
################################################################################

define cabal-install-rule
# $1 - package name
# $2 - package version
$(call package-name,$1,$2): \
    $(HALVM_LIBDIR)/.cabal/packages/hackage.haskell.org/00-index.tar
	$(QUIET_RM)$(RM) -f $$@
	$(QUIET_LIB_MACRO)( cd $(TOPDIR)/libraries && \
	  ( [ -d $1-$2 ] || $(cabal-unpack) $1-$2) && \
	    cd $1-$2                               && \
	    $(cabal-configure)                     && \
	    $(cabal-build)                         && \
	    $(cabal-install-only))

$(call remove-rule,$1,$2)
endef

# Update the index
$(HALVM_LIBDIR)/.cabal/packages/hackage.haskell.org/00-index.tar:
	$(HALVM_CABAL) update

# cereal
$(eval $(call cabal-install-rule,cereal,0.3.0.0))
$(call package-name,cereal,0.3.0.0):        \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,containers,0.3.0.0) \
    $(call package-name,array,0.3.0.1)      \
    $(call package-name,bytestring,0.9.1.7)

# stm
$(eval $(call cabal-install-rule,stm,2.1.2.2))
$(call package-name,stm,2.1.2.2): \
    $(call package-name,base,4.2.0.2) \
    $(call package-name,array,0.3.0.1)

# BoundedChan
$(eval $(call cabal-install-rule,BoundedChan,1.0.0.0))
$(call package-name,BoundedChan,1.0.0.0): \
    $(call package-name,base,4.2.0.2)     \
    $(call package-name,array,0.3.0.1)


################################################################################
# HaLVM Libraries
################################################################################

hs-sources = $(filter-out /dist/,$(shell $(FIND) libraries/$1 -name '*.hs*'))

define build-halvm-rule
# $1 - package name
# $2 - package version
$(call package-name,$1,$2) : $$(call hs-sources,$1)
	$(QUIET_RM)$(RM) -f $$@
	$(QUIET_LIB_MACRO)( cd $(TOPDIR)/libraries/$1 && \
	  $(cabal-configure)                          && \
	  $(cabal-build)                              && \
	  $(cabal-install-only))

$(call remove-rule,$1,$2)
endef

# BitFiddler
$(eval $(call build-halvm-rule,BitFiddler,1.0.0))
$(call package-name,BitFiddler,1.0.0):            \
    $(call package-name,base,4.2.0.2)             \
    $(call package-name,template-haskell,2.4.0.1) \
    $(call package-name,bytestring,0.9.1.7)       \
    $(call package-name,cereal,0.3.0.0)

# communication
$(eval $(call build-halvm-rule,communication,1.0.0))
$(call package-name,communication,1.0.0):   \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,bytestring,0.9.1.7) \
    $(call package-name,containers,0.3.0.0) \
    $(call package-name,syb,0.1.0.2)        \
    $(call package-name,cereal,0.3.0.0)     \
    $(call package-name,HALVMCore,1.0.0)

# HALVMCore
$(eval $(call build-halvm-rule,HALVMCore,1.0.0))
$(call package-name,HALVMCore,1.0.0):       \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,bytestring,0.9.1.7) \
    $(call package-name,syb,0.1.0.2)        \
    $(call package-name,mtl,1.1.0.2)        \
    $(call package-name,BoundedChan,1.0.0.0)

# log4h
$(eval $(call build-halvm-rule,log4h,0.1))
$(call package-name,log4h,0.1):             \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,old-locale,1.0.0.2) \
    $(call package-name,old-time,1.0.0.5)   \
    $(call package-name,HALVMCore,1.0.0)

# RealDevice
$(eval $(call build-halvm-rule,RealDevice,1.0.0))
$(call package-name,RealDevice,1.0.0):      \
    $(call package-name,base,4.2.0.2)       \
    $(call package-name,array,0.3.0.1)      \
    $(call package-name,mtl,1.1.0.2)        \
    $(call package-name,bytestring,0.9.1.7) \
    $(call package-name,stm,2.1.2.2)        \
    $(call package-name,HALVMCore,1.0.0)

# RendezvousLib
$(eval $(call build-halvm-rule,RendezvousLib,1.0.0))
$(call package-name,RendezvousLib,1.0.0):    \
    $(call package-name,base,4.2.0.2)        \
    $(call package-name,bytestring,0.9.1.7)  \
    $(call package-name,communication,1.0.0) \
    $(call package-name,HALVMCore,1.0.0)     \
    $(call package-name,XenDevice,1.0.0)

# XenDevice
$(eval $(call build-halvm-rule,XenDevice,1.0.0))
$(call package-name,XenDevice,1.0.0):        \
    $(call package-name,HALVMCore,1.0.0)     \
    $(call package-name,communication,1.0.0) \
    $(call package-name,base,4.2.0.2)        \
    $(call package-name,containers,0.3.0.0)  \
    $(call package-name,array,0.3.0.1)       \
    $(call package-name,mtl,1.1.0.2)         \
    $(call package-name,bytestring,0.9.1.7)


# ##############################################################################
# integer-gmp
# ##############################################################################

define with-dir
	( cd $1 && $2 )
endef

INTEGER_GMP = $(TOPDIR)/xen-ghc/libraries/integer-gmp

# GMP
GMP_DIR		= $(INTEGER_GMP)/gmp/tarball
GMP_CFLAGS	= $(CONF_FLAGS)

$(GMP_DIR)/gmp-4.2.4: $(GMP_DIR)/gmp-4.2.4-nodoc-patched.tar.bz2
	$(call with-dir,$(GMP_DIR),$(TAR) -jxvf $<)

$(HALVM_LIBDIR)/libgmp.a: $(GMP_DIR)/gmp-4.2.4
	$(call with-dir,$(GMP_DIR)/gmp-4.2.4,                   \
	    ABI="$(GMP_ABI)" CFLAGS="$(GMP_CFLAGS)" ./configure \
	    --disable-shared --enable-static)
	$(MAKE) all -C $(GMP_DIR)/gmp-4.2.4
	$(CP) $(GMP_DIR)/gmp-4.2.4/.libs/libgmp.a $@

# integer-gmp
$(INTEGER_GMP)/cbits/mkGmpDerivedConstants: \
    $(INTEGER_GMP)/cbits/mkGmpDerivedConstants.c
	$(PLATFORM_GHC) -o $@ $<

$(INTEGER_GMP)/cbits/GmpDerivedConstants.h: \
    $(INTEGER_GMP)/cbits/mkGmpDerivedConstants
	$< > $@

$(INTEGER_GMP)/cbits/gmp-wrappers.c:           \
    $(INTEGER_GMP)/cbits/GmpDerivedConstants.h \
    $(INTEGER_GMP)/cbits/gmp-wrappers.cmm
	$(PLATFORM_GHC) -c -keep-hc-file $(INTEGER_GMP)/cbits/gmp-wrappers.cmm
	mv $(INTEGER_GMP)/cbits/gmp-wrappers.hc $@

$(call package-name,integer-gmp,0.2.0.1): \
    $(call package-name,ghc-prim,0.2.0.0) \
    $(INTEGER_GMP)/cbits/gmp-wrappers.c   \
    $(HALVM_LIBDIR)/libgmp.a
	$(QUIET_RM)$(RM) -f $@
	$(call with-dir,$(INTEGER_GMP),$(cabal-configure))
	$(call with-dir,$(INTEGER_GMP),$(cabal-build))
	$(call with-dir,$(INTEGER_GMP),$(cabal-install-only))


# ###########################################################################
#
#  Install!
#
#

PATH_CORRECTABLE_FILES = $(INSTALL_LIBDIR)/cabal.conf

.PHONY: install
install:
	[ -d $(INSTALL_BINDIR) ] || $(MKDIR) -p $(INSTALL_BINDIR)
	[ -d $(INSTALL_LIBDIR) ] || $(MKDIR) -p $(INSTALL_LIBDIR)
	[ -d $(INSTALL_DOCDIR) ] || $(MKDIR) -p $(INSTALL_DOCDIR)
	[ -d $(INSTALL_EXMDIR) ] || $(MKDIR) -p $(INSTALL_EXMDIR)
	$(CHMOD) 0755 $(HALVM_BINDIR)/*
	$(CP)    $(HALVM_BINDIR)/* $(INSTALL_BINDIR)/
	$(CP) -r $(HALVM_LIBDIR)/* $(INSTALL_LIBDIR)/
	$(CP) -r $(HALVM_DOCDIR)   $(INSTALL_DOCDIR)
	$(CP) -r $(HALVM_EXMDIR)   $(INSTALL_EXMDIR)
	$(CP) $(PLATFORM_LIB_PATH)/hsc2hs $(INSTALL_LIBDIR)/hsc2hs.bin
	$(TAIL) -n +11 $(INSTALL_LIBDIR)/hsc2hs > $(INSTALL_LIBDIR)/hsc2hs.tmp2
	@$(ECHO) "#!/bin/sh"                    > $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "exedir='$(INSTALL_LIBDIR2)'" >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "exeprog='hsc2hs.bin'"        >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "executablename=\"\$$exedir/\$$exeprog\"" \
                                           >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "datadir='$(INSTALL_EXMDIR2)'">> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "bindir='$(INSTALL_BINDIR2)'" >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "topdir='$(INSTALL_LIBDIR2)'" >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "pgmgcc='$(GCC)'"             >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(ECHO) "HSC2HS_EXTRA=''"             >> $(INSTALL_LIBDIR)/hsc2hs.tmp1
	@$(CAT) $(INSTALL_LIBDIR)/hsc2hs.tmp1 $(INSTALL_LIBDIR)/hsc2hs.tmp2 > \
	  $(INSTALL_LIBDIR)/hsc2hs
	$(CHMOD) 0755 $(HALVM_LIBDIR)/hsc2hs
	@$(RM) $(INSTALL_LIBDIR)/hsc2hs.tmp?
	for f in $(PATH_CORRECTABLE_FILES) `ls $(INSTALL_LIBDIR)/*/*.conf`; do \
	  $(SED) -i "s!$(HALVM_LIBDIR)!$(INSTALL_LIBDIR2)!g" $$f;               \
	  $(SED) -i "s!$(HALVM_DOCDIR)!$(INSTALL_DOCDIR2)!g" $$f;               \
	  $(SED) -i "s!$(shell pwd)/dist!$(INSTALL_PREFIX2)!g" $$f;             \
	done

