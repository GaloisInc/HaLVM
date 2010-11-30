#!/bin/bash -e
# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND

export PATH=/sbin:/usr/sbin:$PATH

# Path to disk image that we will create
out=$1
shift

# Size of our disk image in bytes
size=$1
shift

# File with grub kernel and module lines
grublines=$1
shift

# ... the rest of the arguments are kernel and module files

tmp=$(mktemp -d)
trap "rm -rf $tmp" 0
# Temporary disk image file
disk=$tmp/disk.img
dd if=/dev/zero of=$disk bs=$size count=1

# Offset of first partition
p0offset=32256
partition=$tmp/partition

# Create partition information

count=$(expr $size / 1024)
heads=240
sectors=63

cylinders=$(expr $size / $heads / 63)

fdisk -C $cylinders -S $sectors -H $heads $disk <<EOF || true
n
p
1


w
EOF
mnt=$tmp/mnt
# Configure grub
mkdir -p $mnt/boot/grub

# Guess location of stages.
# Perhaps this should be a parameter or taken from a configuration instead.
guess(){
  for i in $*; do 
    if [ -d $i ]
    then 
      echo $i
      return 
    fi
  done
  false
}

case $(uname -m) in
    x86_64) ARCH=x86_64;;
	 *) ARCH=i386;;
esac

# Create file system and populate it
# genext2fs -d $mnt -b $(expr \( $size - $p0offset \) / 1024) $partition
dd if=/dev/zero of=$partition count=1 bs=$(expr $size - $p0offset )
mke2fs $partition << EOF
y
EOF

mkdir -p $mnt
mount -o loop $partition $mnt
mkdir -p $mnt/boot/grub

STAGEDIR=$(guess /lib/grub/$ARCH-pc /usr/lib/grub/$ARCH-pc /usr/share/grub/$ARCH-redhat)
# Get stages
cp $STAGEDIR/stage{1,2} $mnt/boot/grub/

cat - $grublines > $mnt/boot/grub/menu.lst <<EOF
timeout 0
serial --unit=0 --speed=9600 --word=8 --parity=no --stop=1
terminal --dumb serial
title Xen / Payload
root (hd0,0)
EOF
(cd $mnt/boot/grub/ && ln ./menu.lst grub.conf)

# Put the payload on the disk
for f in $*; do 
  cp $f $mnt/boot/$(basename $f)
done

umount $mnt

# echo $(expr \( $size - $p0offset \) / 512)
dd if=$partition of=$disk seek=63 bs=512 count=$(expr \( $size - $p0offset \) / 512)
grub --batch --device-map=/dev/null <<EOF
device (hd0) $disk
root (hd0,0)
setup (hd0)
quit
EOF

#tune2fs -j $partition

# Paste partition into disk

# Install boot sector.
# Copy onto target disk image
cp $disk $out || (rm -f $out && false)
