#include <stdlib.h>
#include <stdio.h>
#include <mntent.h>

char *hasmntopt(const struct mntent *mnt __attribute__ ((unused)),
    const char *opt __attribute__ ((unused))) {
  return NULL;
}

int endmntent(FILE *fp __attribute__ ((unused))) {
  return 1;
}

struct mntent *getmntent(FILE *fp __attribute__ ((unused))) {
  return NULL;
}

int addmntent(FILE *fp __attribute__ ((unused)),
    const struct mntent *mnt __attribute__ ((unused))) {
  return 1;
}

struct mntent *getmntent_r(FILE *fp __attribute__ ((unused)),
    struct mntent *mntbuf __attribute__ ((unused)),
    char *buf __attribute__ ((unused)),
    int buflen __attribute__ ((unused))) {
  return NULL;
}

FILE *setmntent(const char *filename __attribute__ ((unused)),
    const char *type __attribute__ ((unused))) {
  return NULL;
}
