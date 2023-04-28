#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

#define SIZE (8*1024*1024)
#define ADDR ((char*)0x70000000)
#define ADDR_END (ADDR + SIZE)

static int    init   = 0;
static char * ptr    = ADDR;

static void ensure_init(void)
{
  if (init) return;

  void *mem = mmap(ADDR, SIZE, PROT_EXEC|PROT_WRITE|PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (mem != ADDR) {
    fprintf(stderr, "Failed to map memory to %x in malloc_shim\n", (size_t)ADDR);
    exit(42);
  }

  init = 1;
}

void *malloc(size_t sz)
{
  ensure_init();

  sz = (sz+16)&~15; /* align up */

  size_t left = ADDR_END - ptr;
  if (sz > left) abort(); /* out-of-memory */

  char *ptr_ret = ptr;
  ptr += sz;

  return ptr;
}

void *calloc(size_t num, size_t sz)
{
  void *ret = malloc(sz);
  memset(ret, 0, sz);
  return ret;
}

void free(void *)
{
  /* just ignore frees */
}
