#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define FILE_SIZE (32 * 1024 * 1024)
#define NUM_BLOCKS (FILE_SIZE / 512)

int main(int argc, char **argv)
{
  void *block = malloc(512);
  unsigned char filler = 0;
  int i;

  for(i = 0; i < NUM_BLOCKS; i++) {
    memset(block, filler, 512);
    write(STDOUT_FILENO, block, 512);
    filler += 1;
  }

  return 0;
}
