# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND

include ../../mk/common.mk

# Not included because they use Qemu: 
# QEMUTESTDIRS  = Applications/DomainBuilder \

# Status:
#   Passing:
#     Applications/ChatDemo
#     Applications/Filter
#     Applications/Xenstore
#     IVC/CopyTest
#     IVC/SpeedTest
#     IVC/TransferTest
#     Misc/ExitTest
#     Misc/GetIdTest
#     Misc/Null
#     Misc/Time
#     PseudoDevice/DoubleDevice
#     PseudoDevice/ROT13Device
#     XenDevice/ARPSniffer
#     XenDevice/EtherPing
#     XenDevice/VBDTest
#     Misc/hypervisorConsole 
#     RealDevice/TestPCI
#
#   Failing:
#
#   Skipping (deferring for now):
#     libIVC/CryptoDevice             <= these all hang after initial
#     libIVC/CryptoDevice2               connection; problem likely in libIVC
#     libIVC/ROT13Echo          	 itself 
#     libIVC/SysLog              
#     XenDevice/BasicHalfs            <= awaiting new Halfs from JS
#
#   QEMU-based tests:
#     Applications/DomainBuilder
#     Misc/XSM
#     RealDevice/TestIDE
#     RealDevice/TestNet
#
TESTDIRS      = Applications/ChatDemo      \
         	Applications/Filter 	   \
         	Applications/Xenstore      \
         	IVC/CopyTest               \
	        IVC/SpeedTest              \
       		IVC/TransferTest 	   \
         	Misc/ExitTest              \
         	Misc/GetIdTest             \
         	Misc/Null                  \
         	Misc/Time                  \
         	PseudoDevice/DoubleDevice  \
         	PseudoDevice/ROT13Device   \
         	XenDevice/ARPSniffer       \
         	XenDevice/EtherPing        \
         	XenDevice/VBDTest          \
         	libIVC/CryptoDevice        \
         	libIVC/CryptoDevice2       \
         	libIVC/ROT13Echo           \
         	libIVC/SysLog              \
         	XenDevice/BasicHalfs       \
		Applications/DomainBuilder \
		Misc/hypervisorConsole     \
		Misc/XSM                   \
		RealDevice/TestIDE         \
		RealDevice/TestNet         \
		RealDevice/TestPCI         \
		RealDevice/TestPCI_Dom0


# These tests may end up being run from static-bits/examples, or dist/ 
TESTING_TOPDIR=$(shell pwd)
TEST_TMPDIR=test_report

# Default values for the files used to record various aspects of the tests
CHECK=check
RESULTS=results
REPORT=report
TOPOUT=top.out
TOPERR=top.err

.PHONY: tests examples build tidy clean

tests:
	@$(ECHO) "Testing:"
	sudo ./../../dist/bin/make_halvm_dir.py
	@for TESTDIR in $(TESTDIRS); do					\
	 $(call make_in_dir,$$TESTDIR,test,$(TEST_TMPDIR),		\
	  $(call test_report,$$TESTDIR,$$TESTDIR/$(TEST_TMPDIR)));	\
	done

examples:
	@$(ECHO) "Building examples:"
	@for TESTDIR in $(TESTDIRS); do					\
	 $(call make_in_dir,$$TESTDIR,example,$(TEST_TMPDIR),		\
	  $(call no_make_errors_in_output,$$TESTDIR/$(TEST_TMPDIR)));	\
	done

build:
	@$(ECHO) "Building examples and test harnesses:"
	@for TESTDIR in $(TESTDIRS); do					\
	 $(call make_in_dir,$$TESTDIR,build,$(TEST_TMPDIR),		\
	  $(call no_make_errors_in_output,$$TESTDIR/$(TEST_TMPDIR)));	\
	done

tidy:
	@$(ECHO) "Tidying up ..."
	@$(ECHO) "Destroying all VMs:"
	@for TESTDIR in $(TESTDIRS); do					\
	 $(call make_in_dir,$$TESTDIR,tidy,$(TEST_TMPDIR),		\
	  "$(ECHO) \"ok\"");						\
	done

clean:
	@$(ECHO) "Cleaning:"
	@for TESTDIR in $(TESTDIRS); do					\
	 $(call make_in_dir,$$TESTDIR,clean,$(TEST_TMPDIR),		\
	  $(call empty_error_output,$$TESTDIR/$(TEST_TMPDIR)));		\
	done


# =======================================================================
#
#   Auxiliary functions
#
# =======================================================================

# =======================================================================
#
# make_in_dir ( <test target dir (relative to testing top dir)>
#             , <make target>
#             , <test output dir (relative to test target dir)>
#             , <success criterion>
#             )
define make_in_dir
$(RM) -rf "$1/$3";							\
$(MKDIR) -p "$1/$3";							\
$(ECHO) -n "  $1: ";							\
if [ -f "$1/.skip" ] ; then						\
    $(ECHO) "skipping";							\
else									\
    OUTDIR="$3" RESULTSFILE="$(RESULTS)" CHECKFILE="$(CHECK)"		\
    REPORTFILE="$(REPORT)" TESTTOPDIR="$(TESTING_TOPDIR)"		\
      $(MAKE) --quiet --no-print-directory -C "$1" "$2"			\
      > "$1/$3/$(TOPOUT)" 2> "$1/$3/$(TOPERR)";				\
    eval "$4";								\
fi
endef

# =======================================================================
#
# test_report ( <test target dir (relative to testing top dir)>
#             , <test output dir>
#             )
define test_report
if [ -s "$2/$(REPORT)" ] ; then						\
    $(ECHO) -n "failed";						\
    if [ -e "$1/.fail" ] ; then						\
        $(ECHO) \" (expected)\";					\
    else								\
        $(ECHO) \" with:\";						\
        $(CAT) "$2/$(CHECK)";						\
    fi									\
else									\
    if $(GREP) -qs "^make\[[0-9]*\]: \*\*\*.*Error [0-9]*$$"		\
                   "$2/$(TOPERR)"; then					\
        $(ECHO) -n "failed";						\
        if [ -e "$1/.fail" ] ; then					\
            $(ECHO) \" (expected)\";					\
	else								\
            $(ECHO) \" with:\";						\
            $(ECHO) "=======";						\
            $(ECHO) "stdout:";						\
            $(ECHO) "=======";						\
            $(CAT) "$2/$(TOPOUT)";					\
            $(ECHO) "=======";						\
            $(ECHO) "stderr:";						\
            $(ECHO) "=======";						\
            $(CAT) "$2/$(TOPERR)";					\
        fi								\
    else								\
	if [ -e "$1/.fail" ] ; then					\
	    $(ECHO) \"ok (failure expected)\";				\
	else								\
	    $(ECHO) "ok";						\
	fi								\
    fi									\
fi
endef

# =======================================================================
#
# empty_error_output ( <test output dir>
#                    )
#
define empty_error_output
if [ -s "$1/$(TOPERR)" ] ; then						\
    $(ECHO) "failed with:";						\
    $(ECHO) "=======";							\
    $(ECHO) "stdout:";							\
    $(ECHO) "=======";							\
    $(CAT) "$1/$(TOPOUT)";						\
    $(ECHO) "=======";							\
    $(ECHO) "stderr:";							\
    $(ECHO) "=======";							\
    $(CAT) "$1/$(TOPERR)";						\
else									\
    $(ECHO) "ok";							\
fi
endef

# =======================================================================
#
# no_make_errors_in_output ( <test output dir>
#                          )
#
define no_make_errors_in_output
if $(GREP) -qs "^make\[[0-9]*\]: \*\*\*.*Error [0-9]*$$" "$1/$(TOPERR)";\
then									\
    $(ECHO) "failed with:";						\
    $(ECHO) "=======";							\
    $(ECHO) "stdout:";							\
    $(ECHO) "=======";							\
    $(CAT) "$1/$(TOPOUT)";						\
    $(ECHO) "=======";							\
    $(ECHO) "stderr:";							\
    $(ECHO) "=======";							\
    $(CAT) "$1/$(TOPERR)";						\
else									\
    $(ECHO) "ok";							\
fi
endef

