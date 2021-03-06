#ifndef _LAMBDACHINE_CAPABILITY_H
#define _LAMBDACHINE_CAPABILITY_H

#include "Common.h"
#include "VM.h"

#include "Jit.h"
#include "Opts.h"

typedef int AsmFunction;

#define DISPATCH_TABLE_LEN   64

typedef enum {
  CF_NO_JIT = 1
} CapabilityFlag;

/* The VM state associated with an OS thread. */
struct Capability_ {
  Thread   *T;                 /* Currently running thread. */
  u4        flags;
  Closure  *static_objs;        /* A linked list of updated static
                                   objects.  These must be GC roots. */
#if LC_HAS_JIT
  JitState  J;
  HotCount  hotcount[HOTCOUNT_SIZE]; /* Hot counters. */
#endif
};

extern Capability* G_cap0;

#define hotcount_hash(pc) \
  (((u4ptr(pc)>>12) ^ (u4ptr(pc)>>4)) & (HOTCOUNT_SIZE - 1))

#define hotcount_get(cap, pc) \
  (cap)->hotcount[hotcount_hash(pc)]
#define hotcount_set(cap, pc, val) \
  (hotcount_get((cap), (pc)) = (HotCount)(val))

void *allocate(Capability *cap, u4 num_words);

void initVM(const Opts* opts);
void initialiseCapability(Capability *cap, const Opts* opts);

#endif
