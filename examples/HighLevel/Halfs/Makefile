BINARIES=Halfs
include ../../standard.mk

run: $(BINARIES)
	sudo xl create $(BINARIES).config
	sleep 1
	sudo xl dmesg -c

mkdisk:
	dd if=/dev/zero of=new.disk bs=1 count=1 seek=64M
