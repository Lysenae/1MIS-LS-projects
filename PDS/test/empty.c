#include <arpa/inet.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ifaddrs.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/if_link.h>

int main()
{
  unsigned short x = 1;
  printf("%u\n", ntohs(htons(x)));

  return 0;
}
