BINARIES=SimplerTime
TIMEOUT?=40
include ../../standard.mk

run: SimplerTime
	sudo xl create SimplerTime.config
	sleep $(TIMEOUT)
	sudo xl dmesg -c
