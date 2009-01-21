#include "stub.h"
#include <stdio.h>
#include <string.h>
#include <cmph.h>

unsigned long stub_hash(cmph_t* hash, char * word) {
  //  fprintf(stderr, "looking up %s\n", word);
  // fprintf(stderr, "hash is %ud\n", hash);
  unsigned long id = cmph_search(hash, word, strlen(word));
  // fprintf(stderr, "looking up %s, got %d\n", word, id);
  return id;
}

void main2() {
  // pull in words
  char * vector[1000000];
  char tmp[1000];
  int count = 0;
  FILE * f = fopen("/usr/share/dict/words", "r");
  while(fgets(tmp, 1000, f)) {
    vector[count++] = strdup(tmp);
  }
  
  build_hash(vector, count);

}


cmph_t * build_hash(char ** vector, int nkeys) {
  fprintf(stderr, "building hash with %d keys, first is %s\n", nkeys, *vector);

  // so this is a massive hack, but we really need this vector to be stable
  // and live in C-land, so we're going to copy it. GROTTY.
/*   char ** copy = malloc(sizeof(char *) * nkeys); */
/*   char ** tmp = copy; */
/*   while(*vector) { */
/*     *tmp = strdup(*vector); */
/*     vector++; tmp++; */
/*   } */
/*   *tmp = NULL; */
  
/*   while(*tmp) { */
/*     fprintf(stderr, "|%s|\n", *tmp); */
/*     tmp++; */
/*   } */
  cmph_io_adapter_t *source = cmph_io_vector_adapter(vector, nkeys);
  fprintf(stderr, "building adapter is %ud\n", source);
  cmph_config_t *config = cmph_config_new(source);
  fprintf(stderr, "building config is %ud\n", config);
  cmph_config_set_algo(config, CMPH_BMZ);
  cmph_t *hash = cmph_new(config);
    // cmph_config_destroy(config);
  fprintf(stderr, "building hash is %ud\n", hash);
  return hash;
}
    
