// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#include <libIVC.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

extern int asprintf (char **__restrict __ptr,
                     __const char *__restrict __fmt, ...);

char *haskell_to_c_string(char *data, int max)
{
  // The HaLVM sends out strings as a length in ASCII, a space, and then
  // the string. So cheat a bit; find the first space, then memmove the
  // data from this position back to the original place.
  char *cur;
  int index;

  for(cur = data, index = 0; index < max; cur++, index++) {
    if(cur[0] == ' ') {
      memmove(data, cur + 1, max - (index + 1));
      for(index = max - (index + 1); index < max; index++) {
        data[index] = 0;
      }

      return data;
    }
  }

  free(data);
  return strdup("BAD VALUE");
}

char *c_to_haskell_string(char *data)
{
  char *res = NULL;
  asprintf(&res, "%i %s", strlen(data), data);
  return res;
}

char *rot13string(char *str)
{
  int i;

  for(i = 0; i < strlen(str); i++)
    if(str[i] >= 'a' && str[i] <= 'z') 
      str[i] = 'a' + (((str[i] - 'a') + 13) % 26);
    else if(str[i] >= 'A' && str[i] <= 'Z')
      str[i] = 'A' + (((str[i] - 'A') + 13) % 26);
  return str;
}

int main(int argc, char **argv)
{
  int res;
  inout_chan *chan;

  initialize_libIVC_library(); 
  printf("libIVC initialized\n");
  chan = connect_two_way("c_rot13");
  printf("Connected!\n");
  while(1) {
    char *input_line = NULL, *output_line = NULL;
    int len;

    len = read_unknown_chan(chan, (void**)&input_line);
    if(!len) {
      printf("Error reading from input channel!\n");
      abort();
    }
    input_line = haskell_to_c_string(input_line, len);
    printf("input: %s\n", input_line);
    input_line = rot13string(input_line);
    printf("rot13: %s\n", input_line);
    output_line = c_to_haskell_string(input_line);
    res = write_chan(chan, output_line, strlen(output_line));
    printf("res: %d\n", res);
    free(input_line); free(output_line);
  }
}
