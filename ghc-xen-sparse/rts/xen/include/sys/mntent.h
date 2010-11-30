#ifndef MNTENT_H
#define MNTENT_H

struct mntent {
  char *mnt_fsname;
  char *mnt_dir;
  char *mnt_type;
  char *mnt_opts;
  int mnt_freq;
  int mnt_passno;
};

char *hasmntopt(const struct mntent *mnt, const char *opt);

int endmntent(FILE *fp);

struct mntent *getmntent(FILE *fp);

int addmntent(FILE *fp, const struct mntent *mnt);

struct mntent *getmntent_r(FILE *fp, struct mntent *mntbuf,
                           char *buf, int buflen);

FILE *setmntent(const char *filename, const char *type);

#endif
