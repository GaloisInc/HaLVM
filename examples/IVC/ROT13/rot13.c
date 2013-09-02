#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libIVC.h"

void translateMessage(char *str)
{
  int i;

  for(i = 0; str[i]; i++) {
    if( (str[i] >= 'A') && (str[i] <= 'Z') )
      str[i] = (((str[i] - 'A') + 13) % 26) + 'A';
    if( (str[i] >= 'a') && (str[i] <= 'z') )
      str[i] = (((str[i] - 'a') + 13) % 26) + 'a';
  }
}

int main(int argc, char **argv)
{
  libIVC_t *ivc = openIVCLibrary();
  ivc_connection_t *ch = makeConnection(ivc,"rot13",ivcInputOutputChannel,0.5);
  int ok = 1;

  while(ok) {
    struct tm *now;
    time_t nowt;
    char *msg;

    msg = (char*)getData(ch);
    translateMessage(msg);
    if( strcmp(msg, "") == 0 ) {
      ok = 0;
    } else {
      putData(ch, msg, strlen(msg) + 1); /* remember the null! */
    }
    free(msg);
  }

  return 0;
}
