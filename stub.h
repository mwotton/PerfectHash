#ifndef __HASH_STUB_H__
#define __HASH_STUB_H__

#include <cmph.h>

unsigned long stub_hash(cmph_t* hash, char * word);
cmph_t * build_hash(char ** vector, int nkeys);
#endif
