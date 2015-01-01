#include <stdio.h>

static char buffer[4096];

int main(int argc, char **argv)
{
  FILE *infile, *outfile;
  size_t size, done;
  int i;

  /* check the argument count */
  if(argc != 3) {
    printf("USAGE: convert-profile <disk> <file>\n");
    return -1;
  }

  /* open the input file */
  infile = fopen(argv[1], "r");
  if(!infile) {
    printf("ERROR: Couldn't open input file.\n");
    return -2;
  }

  /* get the status block */
  size = fread(buffer, 1, 512, infile);
  if(size != 512) {
    printf("ERROR: Short read on status block!\n");
    return -4;
  }

  size = ((size_t*)buffer)[0];
  done = ((size_t*)buffer)[1];
  for(i = 2; i < (512 / sizeof(size_t)); i += 2) {
    if( ((size_t*)buffer)[i] != (size ^ i) ) {
      printf("ERROR: Header block corrupted (size/%d). Giving up.\n", i);
      return -5;
    }
    if( ((size_t*)buffer)[i+1] != (done ^ i) ) {
      printf("ERROR: Header block corrupted (done/%d). Giving up.\n", i);
      return -6;
    }
  }

  if(!done)
    printf("WARNING: Profile not cleanly closed.\n");

  outfile = fopen(argv[2], "w");
  if(!outfile) {
    printf("ERROR: Couldn't open output file.\n");
    return -3;
  }

  printf("Transferring %d bytes\n", size);
  while(size > 0) {
    size_t amount = (size > 4096) ? 4096 : size;
    done = fread(buffer, 1, amount, infile);
    size -= fwrite(buffer, 1, done, outfile);
  }
  fclose(infile);
  fclose(outfile);

  return 0;
}
