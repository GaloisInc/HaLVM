#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "libIVC.h"
#include <assert.h>

int main(int argc, char **argv)
{
  libIVC_t *ivc = openIVCLibrary();
  ivc_connection_t *inch = makeConnection(ivc, "logger", ivcInputChannel, 0.0);
  char *datebuffer = malloc(128);
  int ok = 1;

  while(ok) {
    struct tm *now;
    time_t nowt;
    char *msg;

    msg = (char*)getData(inch);
    assert( nowt = time(NULL) );
    assert( now = localtime(&nowt) );
    assert( strftime(datebuffer, 128, "%c", now) > 5 );
    printf("[%s] %s\n", datebuffer, msg);
    ok = strcmp(msg, "Final event.") != 0;
    free(msg);
  }

  return 0;
}
