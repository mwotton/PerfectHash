#include "stub.h"
#include <stdio.h>
#include <string.h>
#include <cmph.h>

/* alas, BDZ is not defined in 0.7, so we need a fallback */
#ifdef CMPH_BDZ
#define CMPH_ALGO CMPH_BDZ
#else
#define CMPH_ALGO CMPH_BMZ
#endif

cmph_t * build_hash(char ** vector, int nkeys) {
  //  fprintf(stderr, "building hash with %d keys, first is %s, last is %s\n", nkeys, *vector, vector[nkeys-1]);
  cmph_io_adapter_t *source = cmph_io_vector_adapter(vector, nkeys);
  //fprintf(stderr, "building adapter is %p\n", source);
  cmph_config_t *config = cmph_config_new(source);
  //  fprintf(stderr, "building config is %p\n", config);
  cmph_config_set_algo(config, CMPH_ALGO);
  cmph_t *hash = cmph_new(config);
    // cmph_config_destroy(config);
  //  fprintf(stderr, "building hash is %p\n", hash);
  // monkey_debug(vector, nkeys);
  if(!hash) { fprintf(stderr, "Dying horribly, hash is null"); exit(1); } 
  return hash;
}
    

