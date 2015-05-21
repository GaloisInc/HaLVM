BINARIES=Halfs
include ../../standard.mk

run: new.disk Halfs
	sudo xl create Halfs.config
	sleep 1
	sudo xl dmesg -c

new.disk:
	dd if=/dev/zero of=$@ bs=1M count=64
