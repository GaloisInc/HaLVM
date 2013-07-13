// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Iavor Diatchki <diatchki@galois.com> 
// BANNEREND
#include <io.h>
#include <stdio.h>

// do we need this?
// void slow() { __asm__ __volatile__ ("jmp 1f; 1: jmp 1f; 1:" : : ); }
#define slow() 

Word8 in8(Port port) { 
  Word8 value;
  __asm__ __volatile__ ("inb %%dx, %%al" : "=a"(value) : "d"(port)); 
  slow();
  return value; 
}  

Word16 in16(Port port) { 
  Word16 value;          
  __asm__ __volatile__ ("inw %%dx, %%ax" : "=a"(value) : "d"(port)); 
  slow();
  return value; 
}  

Word32 in32(Port port) { 
  Word32 value;          
  __asm__ __volatile__ ("inl %%dx, %%eax" : "=a"(value) : "d"(port)); 
  slow();
  return value; 
}  

void out8 (Port port, Word8 value) { 
  __asm__ __volatile__ ("outb %%al, %%dx" : : "d"(port), "a"(value));
  slow();
} 

void out16 (Port port, Word16 value) { 
  __asm__ __volatile__ ("outw %%ax, %%dx" : : "d"(port), "a"(value));
  slow();
} 

void out32 (Port port, Word32 value) { 
  __asm__ __volatile__ ("outl %%eax, %%dx" : : "d"(port), "a"(value));
  slow();
} 

