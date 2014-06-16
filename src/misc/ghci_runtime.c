#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/resource.h>

void runtime_write(size_t len, char *buffer)
{
  int i;

  for(i = 0; i < len; i++)
    putchar(buffer[i]);
}

void runtime_block(unsigned long milliseconds)
{
  usleep(milliseconds * 1000);
}

void runtime_exit(void)
{
  exit(0);
}

void *runtime_alloc(void* start, size_t length, int prot)
{
  return mmap(start, length, prot, MAP_ANONYMOUS, 0, 0);
}

void *runtime_realloc(void *start, int canmove, size_t oldlen, size_t newlen)
{
  return mremap(start, oldlen, newlen, canmove ? MREMAP_MAYMOVE : MREMAP_FIXED);
}

void runtime_free(void* start, size_t length)
{
  munmap(start, length);
}

int runtime_memprotect(void *addr, size_t length, int prot)
{
  return mprotect(addr, length, prot);
}

int runtime_pagesize(void)
{
  return getpagesize();
}

time_t runtime_time()
{
  return time(NULL);
}

int runtime_gettimeofday(struct timeval *tv)
{
  return gettimeofday(tv, NULL);
}

int runtime_rusage(int who, struct rusage *r)
{
  return getrusage(who, r);
}

void registerWaiter(int _usecs, void *_addr) { }
void waitForWaiter() {}
