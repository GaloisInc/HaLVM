// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
typedef unsigned char Word8;
typedef unsigned short Word16;
typedef unsigned int Word32;
typedef Word16 Port;

Word8  in8 (Port port);
Word16 in16(Port port);
Word32 in32(Port port);

void out8  (Port port, Word8  value);
void out16 (Port port, Word16 value);
void out32 (Port port, Word32 value);

