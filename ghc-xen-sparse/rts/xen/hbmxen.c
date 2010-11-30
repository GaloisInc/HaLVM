// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Authors: Adam Wick <awick@galois.com>, Rebekah Leslie <rebekah@galois.com>
// BANNEREND
#include <hbmxen.h>
#include <stdlib.h>
#include <mm.h>
#include <traps.h>
#include <events.h>
#include <gnttab.h>
#include <time.h>
#include <hypercall.h>
#include <stdio.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////
//
// Routines and structures for dealing with the information given to
// us by Xen at startup.
//

start_info_t *start_info;

static void set_start_info(void *si) 
{
  start_info = si;
}

static size_t split_cmd_line(char* rts_opts, char* prog_opts) {
  char* start_pos = (char*)&(start_info->cmd_line);
  char* end_pos   = strstr(start_pos, "rts_args=");
  char* rts_idx   = rts_opts;
  char* prog_idx  = prog_opts;
  size_t res = 0;

  if (end_pos == NULL) { // no RTS arguments specified
    strncpy(prog_opts, start_pos, strlen(start_pos));
    rts_opts = "";
  } else {
    prog_idx  += strlen(strncpy(prog_idx, start_pos, end_pos - start_pos));
    start_pos =  end_pos + 9; // length of "rts_args="
    end_pos   =  strchr(end_pos, ',');

    for(; end_pos != NULL; end_pos = strchr(start_pos, ',')) {
      rts_idx += strlen(strncpy(rts_idx, start_pos, end_pos - start_pos));
      *rts_idx++ = ' ';
      start_pos = end_pos + 1;
      res++;
    }

    end_pos = strchr(start_pos, ' ');

    if (end_pos == NULL) { // no more args, just copy rest of cmd_line
      strncpy(rts_opts, start_pos, strlen(start_pos));
    } else { // more non-rts args, just copy to end_pos
      rts_idx += strlen(strncpy(rts_idx, start_pos, end_pos - start_pos));
      prog_idx += strlen(strncpy(prog_idx, end_pos + 1, strlen(end_pos + 1)));
    }
    res++;
  }
  return res;
}

////////////////////////////////////////////////////////////////////////
//
// Routines and data structures for dealing with Xen shared info
//

static unsigned char shared_info[PAGE_SIZE] __attribute__ ((aligned(PAGE_SIZE))); // the shared info page

shared_info_t *HYPERVISOR_shared_info = (shared_info_t *) shared_info;

static void map_shared_info(void)
{
#ifdef CONFIG_X86_PAE
  pte_t arg = { .pte_high = 0, .pte_low = start_info->shared_info | 7 };
#else
# if defined(__x86_64__)
  pte_t arg = { .pte = start_info->shared_info | 7 };
# else
  pte_t arg = { .pte_low = start_info->shared_info | 7 };
# endif
#endif

  if ( HYPERVISOR_update_va_mapping((unsigned long)shared_info, arg,
				    UVMF_INVLPG) )
    pabort("Failed to map shared_info!!\n");
}

////////////////////////////////////////////////////////////////////////
//
// Entry point from assembly
//
char **environ = NULL;

extern int main(int argc, char *argv[]);

void c_start(void *si) 
{
  printf("Booting HaLVM ... ");

  /* Save a global pointer to the start_info struct. */
  set_start_info(si);

  init_mm(); 
  /* Map the shared_info page into our address space. */
  map_shared_info();
  init_traps();
  init_events();
  init_time();
  init_gnttab();

  /* Compute the maximum amount of memory GHC should use */
  int maxpages = start_info->nr_pages - current_memory_reservation;
  char memtop[256];
  sprintf(memtop, "-M%dm", maxpages / 256);

  int argc = 6;

  /* Split command line args into RTS args and program args */
  size_t len_cmd_line = strlen((char*)&(start_info->cmd_line));
  char* rts_opts      = malloc(len_cmd_line);
  char* prog_opts     = malloc(len_cmd_line);
  argc                += split_cmd_line(rts_opts, prog_opts);

  printf("done.\n");
  /* rts_opts check needed b/c GHC reacts poorly if given an empty argument */
  if(*rts_opts) {
    char* argv[] = {"","+RTS",memtop,"-c",rts_opts,"-RTS",prog_opts,0};
    main(argc, argv);
  } else {
    char* argv[] = {"","+RTS",memtop,"-c","-RTS",prog_opts,0};
    main(argc, argv);
  }
  // we should never get here
  printf("Fell though to c_start exit!\n");
  exit(0);
}

////////////////////////////////////////////////////////////////////////
//
// Exit
//

void do_exit(void)
{
    printf("Do_exit called!\n");
    for ( ;; ) HYPERVISOR_sched_op(SCHEDOP_shutdown, SHUTDOWN_crash);
}

