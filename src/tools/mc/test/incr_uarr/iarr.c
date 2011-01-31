#include <stdlib.h>

int64_t update (int64_t orig, int64_t round) { return orig + round ; }

void incr_array (int64_t *arr, int64_t len, int64_t round)
{
  int64_t i ;
  for (i = 0 ; i < len ; i ++)
    arr[i] = update (arr[i], round) ;
}

int main (int argc, char **argv)
{
  int64_t nrounds, len, r, *arr ;
  if (argc != 3) exit (1) ;
  nrounds = atol (argv[1]) ;
  len = atol (argv[2]) ;
  if (NULL == (arr = calloc (len, sizeof (long)))) exit (1) ;
  for (r = 1 ; r <= nrounds ; r ++)
    incr_array (arr, len, r) ;
  return 0 ;
}
