#include <stdio.h>
#include "InfoTables.h"
#include "PrintClosure.h"
#include "Bytecode.h"

void
printClosure(Closure* cl)
{
  const InfoTable *info = getInfo(cl);

  if (info == NULL) {
    printf("???\n");
    return;
  }

  if (info->type == IND) {
    cl = (Closure*)cl->payload[0];
    info = getInfo(cl);
    printf("IND -> ");
  }

  switch (info->type) {
  case CONSTR:
    printf("%s ", cast(ConInfoTable*,info)->name);
    break;
  case FUN:
    printf("%s ", cast(FuncInfoTable*,info)->name);
    break;
  case THUNK:
    printf("%s ", cast(ThunkInfoTable*,info)->name);
    break;
  }
    
  int n, p = 0;
  for (n = info->layout.payload.ptrs; n > 0; p++, n--)
    printf("%0" FMT_WordLen FMT_WordX " ", cl->payload[p]);
  for (n = info->layout.payload.nptrs; n > 0; p++, n--)
    printf("%" FMT_Int " ", cl->payload[p]);
  
  printf("\n");
  
}

u4
printInstruction(BCIns *ins /*in*/)
{
  BCIns *ins0 = ins;
  u4 j;
  BCIns i = *ins;
  const char *name = ins_name[bc_op(i)];

  printf("%p: ", ins);
  ++ ins;

  switch(ins_format[bc_op(i)]) {
  case IFM_R:
    printf("%s\tr%d\n", name, bc_a(i)); break;
  case IFM_RR:
    printf("%s\tr%d, r%d\n", name, bc_a(i), bc_d(i)); break;
  case IFM_RRR:
    printf("%s\tr%d, r%d, r%d\n", name, bc_a(i), bc_b(i), bc_c(i));
    break;
  case IFM_RN:
    printf("%s\tr%d, %d\n", name, bc_a(i), bc_d(i)); break;
  case IFM_RRN:
    printf("%s\tr%d, r%d, %d\n", name, bc_a(i), bc_b(i), bc_c(i));
    break;
  case IFM_RS:
    printf("%s\tr%d, %d\n", name, bc_a(i), bc_sd(i)); break;
  case IFM_J:
    printf("%s\t%p\n", name, ins + bc_j(i)); break;
  case IFM_RRJ:
    printf("%s\tr%d, r%d, %p\n", name, bc_a(i), bc_d(i),
           ins + 1 + bc_j(*ins));
    ins++;
    break;
  case IFM____:
    switch (bc_op(i)) {
    case BC_EVAL:
      { printf("EVAL\tr%d\n", bc_a(i)); ins++; 
      }
      break;
    case BC_CASE:
      { u2 *tgt = (u2*)ins;  u4 ncases = bc_d(i);
        ins += (ncases + 1) / 2;
        printf("CASE\tr%d\n", bc_a(i));
        for (j = 0; j < ncases; j++, tgt++) {
          printf("         %d: %p\n", j + 1, ins + bc_j_from_d(*tgt));
        }
      }
      break;
    case BC_CASE_S:
      printf("CASE_S\tr%d ...TODO...\n", bc_a(i));
      ins += bc_d(i);
      break;
    case BC_ALLOC:
      {
        u1 *arg = (u1*)ins; ins += (bc_c(i) + 2) / 4;
        printf("ALLOC\tr%d, r%d", bc_a(i), bc_b(i));
        for (j = 0; j < bc_c(i); j++, arg++)
          printf(", r%d", *arg);
        printf("\n");
      }
      break;
    case BC_ALLOCAP:
      {
        u1 *arg = (u1*)ins; ins += (bc_c(i) + 2) / 4;
        printf("ALLOCAP\tr%d, r%d", bc_a(i), bc_b(i));
        for (j = 0; j < bc_c(i); j++, arg++)
          printf(", r%d", *arg);
        printf("\n");
      }
      break;
    case BC_CALL:
    case BC_CALLT:
      { u1 *arg = (u1*)ins; ins += (bc_b(i) + 2) / 4;
        printf("%s\tr%d(r%d", name, bc_a(i), bc_c(i));
        for (j = 1; j < bc_b(i); j++, arg++)
          printf(", r%d", *arg);
        printf(")\n");
      }
      break;
    default:
      printf("%s ...TODO...\n", name);
    }
    break;
  default:
    fprintf(stderr, "FATAL: Unknown intruction format: %d\n",
            ins_format[bc_op(i)]);
  }

  return (u4)(ins - ins0);
}

void
printInfoTable(InfoTable* info0)
{
  switch (info0->type) {
  case CONSTR:
    {
      ConInfoTable* info = (ConInfoTable*)info0;
      printf("Constructor: %s, (%p)\n", info->name, info);
      printf("  tag: %d\n", info->i.tagOrBitmap);
      printf("  ptrs/nptrs: %d/%d\n",
             info->i.layout.payload.ptrs, info->i.layout.payload.nptrs);
    }
    break;
  case FUN:
    {
      FuncInfoTable *info = (FuncInfoTable*)info0;
      printf("Function: %s (%p)\n", info->name, info);
      printf("  ptrs/nptrs: %d/%d\n",
             info->i.layout.payload.ptrs, info->i.layout.payload.nptrs);
      printCode(&info->code);
    }
    break;
  case THUNK:
    {
      ThunkInfoTable *info = (ThunkInfoTable*)info0;
      printf("Thunk: %s (%p)\n", info->name, info);
      printf("  ptrs/nptrs: %d/%d\n",
             info->i.layout.payload.ptrs, info->i.layout.payload.nptrs);
      printCode(&info->code);
    }
    break;
  default:
    printf("Unknown info table\n");
  }
  printf("\n");
}

void
printCode(LcCode *code)
{
  u4 i; u4 nc = 0; BCIns *c = code->code;
  printf("  arity: %d\n", code->arity);
  printf("  frame: %d\n", code->framesize);
  printf("  literals:\n");
  for (i = 0; i < code->sizelits; i++) {
    printf("   %3d: ", i);
    switch (code->littypes[i]) {
    case LIT_INT:
      printf("%" FMT_Int, (WordInt)code->lits[i]);
      break;
    case LIT_STRING:
      printf("\"%s\"", (char*)code->lits[i]);
      break;
    case LIT_CLOSURE:
      printf("clos %" FMT_WordX " (%s)", code->lits[i],
             getFInfo(code->lits[i])->name);
      break;
    case LIT_INFO:
      printf("info %" FMT_WordX " (%s)", code->lits[i],
             cast(FuncInfoTable*,code->lits[i])->name);
      break;
    default:
      printf("???");
    }
    printf("\n");
  }
  printf("  code:\n");
  while (nc < code->sizecode) {
    i = printInstruction(c);
    c += i;
    nc += i;
  }
}
