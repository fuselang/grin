#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#ifdef USE_BOEHM_GC
#include <gc.h>
#else
extern int64_t _heap_ptr_;
#endif

/*
TODO: Add statistics.
*/

int64_t grinMain();

void __runtime_error(int64_t c){
  exit(c);
}

int main() {
#ifdef USE_BOEHM_GC
  // Initialize Boehm GC - all allocations handled by GC_malloc
  GC_INIT();
#else
  // Bump allocator mode - pre-allocate heap and set heap pointer
  int64_t* heap = malloc(100*1024*1024);
  _heap_ptr_ = (int64_t)heap;
#endif

  grinMain();

#ifndef USE_BOEHM_GC
  free(heap);
#endif

  return 0;
}
