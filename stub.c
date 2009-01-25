#include "stub.h"
#include <stdio.h>
#include <string.h>
#include <cmph.h>

// #define __MONKEY_DEBUG__

void monkey_debug(char ** vector, int nkeys) {
  int i = 0;
  int blah=0;
  for(i=0; i< nkeys; i++) {
    fprintf(stderr, "read:%5d: %s\n", i, vector[i]);    
  }
}

cmph_t * build_hash(char ** vector, int nkeys) {
#ifdef __MONKEY_DEBUG__
  monkey_debug(vector, nkeys);
#endif
  //  fprintf(stderr, "building hash with %d keys, first is %s, last is %s\n", nkeys, *vector, vector[nkeys-1]);

  cmph_io_adapter_t *source = cmph_io_vector_adapter(vector, nkeys);
  //fprintf(stderr, "building adapter is %p\n", source);
  cmph_config_t *config = cmph_config_new(source);
  //  fprintf(stderr, "building config is %p\n", config);
  cmph_config_set_algo(config, CMPH_BDZ);
  cmph_t *hash = cmph_new(config);
    // cmph_config_destroy(config);
  //  fprintf(stderr, "building hash is %p\n", hash);
  // monkey_debug(vector, nkeys);
  if(!hash) { fprintf(stderr, "Dying horribly, hash is null"); exit(1); } 
  return hash;
}
    

