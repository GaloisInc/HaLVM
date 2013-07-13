/* BANNERSTART */
/* - Copyright 2006-2008, Galois, Inc. */
/* - This software is distributed under a standard, three-clause BSD license. */
/* - Please see the file LICENSE, distributed with this software, for specific */
/* - terms and conditions. */
/* Author: Andrew Tolmach */
/* BANNEREND */
OUTPUT_FORMAT("elf64-x86-64", "elf64-x86-64", "elf64-x86-64")
OUTPUT_ARCH(i386:x86-64)
ENTRY(_start)
SECTIONS
{
  /* Leave the first 4MB free, so that NULL ptrs etc can be caught. */
  . = 0x00400000;         /*   Probably good to make this 4MB aligned for Xen */
  _text = .;		      /* Text and read-only data */
  .text : {
	*(.text)
	*(.gnu.warning)
	} = 0x9090

  . = ALIGN(4096);
  _etext = .;			/* End of text section */

  .rodata : { *(.rodata) *(.rodata.*) }

  .data : {			/* Data */
	*(.data)
	CONSTRUCTORS
	}

  _edata = .;			/* End of data section */

  . = ALIGN(8192);		/* init_task */
  .data.init_task : { *(.data.init_task) }

  . = ALIGN(4096);
  .data.page_aligned : { *(.data.idt) }

  . = ALIGN(32);
  .data.cacheline_aligned : { *(.data.cacheline_aligned) }

  __bss_start = .;		/* BSS */
  .bss : {
	*(.bss)
	}
  _end = . ;

  /* Sections to be discarded */
  /DISCARD/ : {
	*(.text.exit)
	*(.data.exit)
	*(.exitcall.exit)
	}

  /* Stabs debugging sections.  */
  .stab 0 : { *(.stab) }
  .stabstr 0 : { *(.stabstr) }
  .stab.excl 0 : { *(.stab.excl) }
  .stab.exclstr 0 : { *(.stab.exclstr) }
  .stab.index 0 : { *(.stab.index) }
  .stab.indexstr 0 : { *(.stab.indexstr) }
  .comment 0 : { *(.comment) }
}
