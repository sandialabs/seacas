#include <copy_string.h>
/* Safer than strncpy -- guarantees null termination */
char *copy_string(char *restrict dest, char const *restrict source, size_t elements)
{
  char *d;
  for (d = dest; d + 1 < dest + elements; d++, source++)
    *d = *source;
  *d = '\0';
  return d;
}
