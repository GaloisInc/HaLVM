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

char *haskell_to_c_string(char *data, int max)
{
  // The HaLVM sends out strings as a length in ASCII, a space, and then
  // the string. So cheat a bit; find the first space, then memmove the
  // data from this position back to the original place.
  char *cur;
  int index;

  for(cur = data, index = 0; index < max; cur++, index++)
    if(cur[0] == ' ') {
      memmove(data, cur + 1, max - (index + 1));
      for(index = max - (index + 1); index < max; index++)
        data[index] = 0;
      return data;
    } 
  free(data);
  return strdup("BAD VALUE");
}

int main(int argc, char **argv)
{
  in_chan *src;

  initialize_libIVC_library(); 
  src = connect_one_way_in("c_logger");
  printf("Connected!\n");
  while(1) {
    char *input_line = NULL;
    struct timeval tm;
    int len;
    
    len = read_unknown_inchan(src, (void**)&input_line);
    if(!len) {
      printf("Error reading from input channel!\n");
      abort();
    }
    input_line = haskell_to_c_string(input_line, len);
    if(gettimeofday(&tm, NULL) == 0)  {
      char *timestr = asctime(localtime(&tm.tv_sec));
      timestr[strlen(timestr) - 1] = 0;
      printf("[%s] %s\n", timestr, input_line);
    } else printf("[UNKNOWN] %s\n", input_line);
    free(input_line);
  }
}
