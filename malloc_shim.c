#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define SIZE (8*1024*1024)
static char mem[SIZE];
static char *ptr = mem;

void *malloc(size_t sz)
{
  sz = (sz+16)&~15; /* align up */

  size_t left = SIZE - (ptr - mem);
  if (sz > left) abort(); /* out-of-memory */

  char *ptr_ret = ptr;
  ptr += sz;

  return ptr;
}

void *calloc(size_t num, size_t sz)
{
  size_t total_size = num * sz;
  total_size = (total_size+16)&~15; /* align up */

  /* Call directly to sbrk() on i386 linux */
  /*void *ret = (void*)syscall(45, total_size);*/
  void *ret = malloc(total_size);

  memset(ret, 0, total_size);
  return ret;
}

void free(void *)
{
  /* just ignore frees */
}
