/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2021 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy                                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/config.h"
#include <string.h>
#include <stdio.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <assert.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/gc_ctrl.h"
#include "caml/platform.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/startup_aux.h"
#include "caml/shared_heap.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "caml/frame_descriptors.h"
#endif
#if defined(USE_MMAP_MAP_STACK) || !defined(STACK_CHECKS_ENABLED)
#include <sys/mman.h>
#endif
#ifdef __linux__
/* for gettid */
#include <sys/types.h>
#include <sys/syscall.h>
#endif

#ifdef DEBUG
#define fiber_debug_log(...) caml_gc_log(__VA_ARGS__)
#else
#define fiber_debug_log(...)
#endif

static_assert(sizeof(struct stack_info) == Stack_ctx_words * sizeof(value), "");

static _Atomic int64_t fiber_id = 0;

/* Parameters settable with OCAMLRUNPARAM */
uintnat caml_init_main_stack_wsz = 0;   /* -Xmain_stack_size= */
uintnat caml_init_thread_stack_wsz = 0; /* -Xthread_stack_size= */
uintnat caml_init_fiber_stack_wsz = 0;  /* -Xfiber_stack_size= */

uintnat caml_nohugepage_stacks = 1;

uintnat caml_get_init_stack_wsize (int context)
{
  uintnat init_stack_wsize = 0;

#ifdef STACK_GUARD_PAGES
  switch(context) {
  case STACK_SIZE_MAIN:   init_stack_wsize = caml_init_main_stack_wsz; break;
  case STACK_SIZE_THREAD: init_stack_wsize = caml_init_thread_stack_wsz; break;
  case STACK_SIZE_FIBER:  init_stack_wsize = caml_init_fiber_stack_wsz; break;
  default: caml_fatal_error("caml_get_init_stack_wsize: invalid context");
  }
#else
  switch(context) {
  case STACK_SIZE_MAIN:
  case STACK_SIZE_THREAD: init_stack_wsize = Wsize_bsize(Stack_init_bsize); break;
  case STACK_SIZE_FIBER:  init_stack_wsize = Wsize_bsize(Stack_threshold * 2); break;
  default: caml_fatal_error("caml_get_init_stack_wsize: invalid context");
  }
#endif

  uintnat stack_wsize = 0;
  if (init_stack_wsize < caml_max_stack_wsize)
    stack_wsize = init_stack_wsize;
  else
    stack_wsize = caml_max_stack_wsize;

  /* If we are requesting a large stack (more than a hugepage), then
     we'd like the total allocation size to be a multiple of the huge
     page size. However, the stack guard pages, headers, etc. have
     some overhead, so we want the requested stack size to be a bit
     less than a multiple of the hugepage size */
  if (caml_plat_hugepagesize > 0
      && stack_wsize > Wsize_bsize(caml_plat_hugepagesize)) {
    /* round down to multiple of hugepage size */
    stack_wsize &= ~(Wsize_bsize(caml_plat_hugepagesize) - 1);
    /* 3 pages is enough to cover the overhead */
    stack_wsize -= 3 * Wsize_bsize(caml_plat_pagesize);
  }

  return stack_wsize;
}

void caml_change_max_stack_size (uintnat new_max_wsize)
{
  struct stack_info *current_stack = Caml_state->current_stack;
  asize_t wsize = Stack_high(current_stack) - (value*)current_stack->sp
                 + Stack_threshold / sizeof (value);

  if (new_max_wsize < wsize) new_max_wsize = wsize;
  if (new_max_wsize != caml_max_stack_wsize){
    CAML_GC_MESSAGE(STACKS,
                    "Changing stack limit to %"
                    ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                    Bsize_wsize(new_max_wsize) / 1024);
  }
  caml_max_stack_wsize = new_max_wsize;
}

#define NUM_STACK_SIZE_CLASSES 5

struct stack_info** caml_alloc_stack_cache (void)
{
  int i;

  struct stack_info** stack_cache =
    (struct stack_info**)caml_stat_alloc_noexc(sizeof(struct stack_info*) *
                                               NUM_STACK_SIZE_CLASSES);
  if (stack_cache == NULL)
    return NULL;

  for(i = 0; i < NUM_STACK_SIZE_CLASSES; i++)
    stack_cache[i] = NULL;

  return stack_cache;
}

static void free_stack_memory(struct stack_info*);
void caml_free_stack_cache(struct stack_info** cache)
{
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    while (cache[i] != NULL) {
      struct stack_info* stk = cache[i];
      cache[i] = (struct stack_info*)stk->exception_ptr;
      free_stack_memory(stk);
    }
  }
  caml_stat_free(cache);
}

/* Round up to a power of 2 */
static uintnat round_up_p2(uintnat x, uintnat p2)
{
  CAMLassert (Is_power_of_2(p2));
  return (x + p2 - 1) & ~(p2 - 1);
}

/* Allocate a stack with at least the specified number of words.
   The [handler] field of the result is initialised (so Stack_high(...)) is
   well-defined), but other fields are uninitialised */
Caml_inline struct stack_info* alloc_for_stack (mlsize_t wosize, int64_t id)
{
  /* Ensure 16-byte alignment of the [struct stack_handler*]. */
  const int stack_alignment = 16;

  /* Ensure we have room to offset Stack_high. */
  wosize += Stack_padding_word;

#ifdef USE_MMAP_MAP_STACK
  size_t len = sizeof(struct stack_info) +
               sizeof(value) * wosize +
               8 + /* For 16-byte aligning handler */
               sizeof(struct stack_handler);
  struct stack_info* si;
  si = mmap(NULL, len, PROT_WRITE | PROT_READ,
             MAP_ANONYMOUS | MAP_PRIVATE | MAP_STACK, -1, 0);
  if (si == MAP_FAILED)
    return NULL;

  si->size = len;

  si->handler =
    (struct stack_handler*)
    round_up_p2((uintnat)si + sizeof(struct stack_info)
      + sizeof(value) * wosize, stack_alignment);

  return si;
#elif defined(STACK_GUARD_PAGES)
  /* (We use the following strategy only in native code, because bytecode
   * has its own way of dealing with stack checks.)
   *
   * We want to detect a stack overflow by triggering a segfault when a
   * given part of the memory is accessed; in order to do so, we protect
   * a page near the end of the stack to make it unreadable/unwritable.
   * A signal handler for segfault will be installed, that will check if
   * the invalid address is in the range we protect, and will raise a stack
   * overflow exception accordingly.
   */
  size_t page_size = caml_plat_pagesize;
  size_t len = Bsize_wsize(wosize);
  uintnat trailer_size = round_up_p2(sizeof(struct stack_handler),
    stack_alignment);
  len += trailer_size;

  // We need two more pages for stack_info and guard
  CAMLassert(sizeof(struct stack_info) <= page_size);
  len += 2 * page_size;
  len = caml_mem_round_up_mapping_size(len);

  // Stack layout (higher addresses are at the top):
  //
  // --------------------
  // struct stack_handler
  // -------------------- <- [stack->handler], 16-aligned
  // pad word (amd64-no-fp)
  // -------------------- <- Stack_high
  // the stack itself
  // -------------------- <- page-aligned
  // guard page
  // -------------------- <- page-aligned
  // padding to one page
  // struct stack_info
  // -------------------- <- [stack], page/hugepage-aligned (by caml_mem_map)
  struct stack_info* stack;
  /* These mappings should never use HugeTLB pages, due to the guard page */
  stack = caml_mem_map(len, CAML_MAP_NO_HUGETLB, NULL);
  if (stack == NULL) {
    return NULL;
  }
#ifdef __linux__
  /* On Linux, (optionally) disable *any* hugepage usage for stacks.
     (Huge pages are not as beneficial for stacks, because you use the same few
     kb over and over again, but can have a significant RAM cost) */
  if (caml_nohugepage_stacks) madvise(stack, len, MADV_NOHUGEPAGE);
#endif
  // mmap is always expected to return a page-aligned value.
  CAMLassert((uintnat)stack % page_size == 0);

  if (mprotect(Protected_stack_page(stack), page_size, PROT_NONE)) {
    caml_mem_unmap(stack, len);
    return NULL;
  }

#ifdef __linux__
  /* On Linux, give names to the various mappings */
  caml_mem_name_map(stack, page_size,
                    "stack info (original fiber id %ld, tid %ld)",
                    id, (long)syscall(SYS_gettid));

  caml_mem_name_map(Protected_stack_page(stack), page_size,
                    "guard page for stack (original fiber id %ld, tid %ld)",
                    id, (long)syscall(SYS_gettid));

  caml_mem_name_map(Stack_base(stack), len - 2*page_size,
                    "stack (original fiber id %ld, tid %ld)",
                    id, (long)syscall(SYS_gettid));
#endif

  // Assert that the guard page does not impinge on the actual stack area.
  CAMLassert((char*) stack + len - (trailer_size + Bsize_wsize(wosize))
    >= Protected_stack_page(stack) + page_size);

  stack->size = len;
  stack->handler = (struct stack_handler*)((char*)stack + len - trailer_size);
  CAMLassert(((uintnat) stack->handler) % stack_alignment == 0);

  return stack;
#else
  size_t len = sizeof(struct stack_info)+
               sizeof(value) * wosize +
               8 + /* For 16-byte aligning handler */
               sizeof(struct stack_handler);
  struct stack_info* stack = caml_stat_alloc_noexc(len);
  if (stack == NULL) return NULL;
  stack->handler =
    (struct stack_handler*)
    round_up_p2((uintnat)stack + sizeof(struct stack_info) +
      sizeof(value) * wosize, stack_alignment);
  return stack;
#endif /* USE_MMAP_MAP_STACK, STACK_GUARD_PAGES */
}

/* Returns the index into the [Caml_state->stack_cache] array if this size is
 * pooled. If unpooled, it is [-1].
 *
 * Stacks may be unpooled if either the stack size is not 2**N multiple of
 * [caml_fiber_wsz] or the stack is bigger than pooled sizes. */
Caml_inline int stack_cache_bucket (mlsize_t wosize) {
  mlsize_t size_bucket_wsz = caml_fiber_wsz;
  int bucket=0;

  while (bucket < NUM_STACK_SIZE_CLASSES) {
    if (wosize == size_bucket_wsz)
      return bucket;
    ++bucket;
    size_bucket_wsz += size_bucket_wsz;
  }

  return -1;
}

static struct stack_info*
alloc_size_class_stack_noexc(mlsize_t wosize, int cache_bucket, value hval,
                             value hexn, value heff, int64_t id)
{
  struct stack_info* stack;
  struct stack_info **cache = Caml_state->stack_cache;

  static_assert(sizeof(struct stack_info) % sizeof(value) == 0, "");
  static_assert(sizeof(struct stack_handler) % sizeof(value) == 0, "");

  CAMLassert(cache != NULL);

  if (cache_bucket != -1 &&
      cache[cache_bucket] != NULL) {
    stack = cache[cache_bucket];
    cache[cache_bucket] =
      (struct stack_info*)stack->exception_ptr;
    CAMLassert(stack->cache_bucket == stack_cache_bucket(wosize));
  } else {
    /* couldn't get a cached stack, so have to create one */
    stack = alloc_for_stack(wosize, id);
    if (stack == NULL) {
      return NULL;
    }

    stack->cache_bucket = cache_bucket;
  }

  struct stack_handler* hand = stack->handler;
  hand->handle_value = hval;
  hand->handle_exn = hexn;
  hand->handle_effect = heff;
  hand->parent = NULL;
  stack->sp = Stack_high(stack);
  stack->exception_ptr = NULL;
  stack->id = id;
  stack->local_arenas = NULL;
  stack->local_sp = 0;
  stack->local_top = NULL;
  stack->local_limit = 0;
#ifdef DEBUG
  stack->magic = 42;
#endif
  /* Due to stack alignment performed above, the actual stack size may be
   * larger than requested. */
  CAMLassert(Stack_high(stack) - Stack_base(stack) >= wosize);
  return stack;

}

/* allocate a stack with at least "wosize" usable words of stack */
struct stack_info*
caml_alloc_stack_noexc(mlsize_t wosize, value hval, value hexn, value heff,
                       int64_t id)
{
  int cache_bucket = stack_cache_bucket (wosize);
  return alloc_size_class_stack_noexc(wosize, cache_bucket, hval, hexn, heff,
                                      id);
}

#ifdef NATIVE_CODE

value caml_alloc_stack (value hval, value hexn, value heff) {
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, id);

  if (!stack) caml_raise_out_of_memory();

  fiber_debug_log ("Allocate stack=%p of %" ARCH_INTNAT_PRINTF_FORMAT
                     "u words", stack, caml_fiber_wsz);

  return Val_ptr(stack);
}


void caml_get_stack_sp_pc (struct stack_info* stack,
                           char** sp /* out */, uintnat* pc /* out */)
{
  char* p = (char*)stack->sp;
  p = First_frame(p);
  *pc = Saved_return_address(p); /* ret addr */
  *sp = p;                       /* pointer to first frame */
}


/* Returns the arena number of a block,
   or -1 if it is not in any local arena */
static int get_local_ix(caml_local_arenas* loc, value v)
{
  int i;
  CAMLassert(Is_block(v));
  /* Search local arenas, starting from the largest (last) */
  for (i = 0; i < loc->count; i++) {
    struct caml_local_arena arena = loc->arenas[i];
    if (arena.base <= (char*)v && (char*)v < arena.base + arena.length)
      return i;
  }
  return -1;
}


/* If it visits an unmarked local block,
      returns the index of the containing arena
   Otherwise returns -1.
   Temporarily marks local blocks with colors.GARBAGE
    (which is not otherwise the color of reachable blocks) */
static int visit(scanning_action f, void* fdata,
                 struct caml_local_arenas* locals,
                 struct global_heap_state colors,
                 value* p)
{
  value v = *p, vblock = v;
  header_t hd;
  int ix;
  if (!Is_block(v))
    return -1;

  if (Is_young(v)) {
    f(fdata, v, p);
    return -1;
  }

  /* major or local or external */

  hd = Hd_val(vblock);
  if (Tag_hd(hd) == Infix_tag) {
    vblock -= Infix_offset_val(v);
    hd = Hd_val(vblock);
  }

  if (Color_hd(hd) == colors.GARBAGE) {
    /* Local, marked */
    return -1;
  } else if (Color_hd(hd) == NOT_MARKABLE) {
    /* Local (unmarked) or external */

    if (locals == NULL)
      /* external */
      return -1;

    ix = get_local_ix(locals, vblock);

    if (ix != -1) {
      /* Mark this unmarked local */
      *Hp_val(vblock) = With_status_hd(hd, colors.GARBAGE);
    }

    return ix;
  } else {
    /* Major heap */
    f(fdata, v, p);
    return -1;
  }
}

static void scan_local_allocations(scanning_action f, void* fdata,
                                   caml_local_arenas* loc, uintnat local_sp)
{
  int arena_ix;
  intnat sp;
  struct caml_local_arena arena;
  /* does not change during scanning */
  struct global_heap_state colors = caml_global_heap_state;

  if (loc == NULL) return;
  CAMLassert(loc->count > 0);
  sp = local_sp;
  arena_ix = loc->count - 1;
  arena = loc->arenas[arena_ix];
#ifdef DEBUG
  { header_t* hp;
    for (hp = (header_t*)arena.base;
         hp < (header_t*)(arena.base + arena.length + sp);
         hp++) {
      *hp = Debug_free_local;
    }
  }
#endif

  while (sp < 0) {
    header_t* hp = (header_t*)(arena.base + arena.length + sp), hd = *hp;
    intnat i;

    if (hd == Local_uninit_hd) {
      CAMLassert(arena_ix > 0);
      arena = loc->arenas[--arena_ix];
#ifdef DEBUG
      for (hp = (header_t*)arena.base;
           hp < (header_t*)(arena.base + arena.length + sp);
           hp++) {
        *hp = Debug_free_local;
      }
#endif
      continue;
    }
    CAMLassert(Color_hd(hd) == NOT_MARKABLE ||
               Color_hd(hd) == colors.GARBAGE);
    if (Color_hd(hd) == NOT_MARKABLE) {
      /* Local allocation, not marked */
#ifdef DEBUG
      /* We don't check the reserved bits here because this is OK even for mixed
         blocks. */
      for (i = 0; i < Wosize_hd(hd); i++)
        Field(Val_hp(hp), i) = Debug_free_local;
#endif
      sp += Bhsize_hd(hd);
      continue;
    }
    /* reset mark */
    hd = With_status_hd(hd, NOT_MARKABLE);
    *hp = hd;
    CAMLassert(Tag_hd(hd) != Infix_tag);  /* start of object, no infix */
    CAMLassert(Tag_hd(hd) != Cont_tag);   /* no local continuations */
    if (Tag_hd(hd) >= No_scan_tag) {
      sp += Bhsize_hd(hd);
      continue;
    }
    i = 0;
    if (Tag_hd(hd) == Closure_tag)
      i = Start_env_closinfo(Closinfo_val(Val_hp(hp)));

    mlsize_t scannable_wosize = Scannable_wosize_hd(hd);

    for (; i < scannable_wosize; i++) {
      value *p = Op_val(Val_hp(hp)) + i;
      int marked_ix = visit(f, fdata, loc, colors, p);
      if (marked_ix != -1) {
        struct caml_local_arena a = loc->arenas[marked_ix];
        intnat newsp = (char*)*p - (a.base + a.length);
        if (sp <= newsp) {
          /* forwards pointer, common case */
          CAMLassert(marked_ix <= arena_ix);
        } else {
          /* If backwards pointers are ever supported (e.g. local recursive
             values), then this should reset sp and iterate to a fixpoint */
          CAMLassert(marked_ix >= arena_ix);
          caml_fatal_error("backwards local pointer");
        }
      }
    }
    sp += Bhsize_hd(hd);
  }
}


Caml_inline void scan_stack_frames(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs,
  struct caml_local_arenas* locals)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  value *root;
  caml_frame_descrs fds = caml_get_frame_descrs();
  /* does not change during marking */
  struct global_heap_state colors = caml_global_heap_state;

  sp = (char*)stack->sp;
  regs = gc_regs;

next_chunk:
  if (sp == (char*)Stack_high(stack)) return;
  sp = First_frame(sp);
  retaddr = Saved_return_address(sp);

  while(1) {
    d = caml_find_frame_descr(fds, retaddr);
    CAMLassert(d);
    if (!frame_return_to_C(d)) {
      /* Scan the roots in this frame */
      if (frame_is_long(d)) {
        frame_descr_long *dl = frame_as_long(d);
        uint32_t *p;
        uint32_t n;
        for (p = dl->live_ofs, n = dl->num_live; n > 0; n--, p++) {
          uint32_t ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          visit (f, fdata, locals, colors, root);
        }
      } else {
        uint16_t *p;
        uint16_t n;
        for (p = d->live_ofs, n = d->num_live; n > 0; n--, p++) {
          uint16_t ofs = *p;
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          visit (f, fdata, locals, colors, root);
        }
      }
      /* Move to next frame */
      sp += frame_size(d);
      retaddr = Saved_return_address(sp);
      /* XXX KC: disabled already scanned optimization. */
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous
       * stack chunk.  */
      regs = Saved_gc_regs(sp); /* update gc_regs */
      sp += Stack_header_size;  /* skip trap frame, gc_regs, DWARF pointer */
      goto next_chunk;
    }
  }
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs)
{
  while (stack != NULL) {
    caml_local_arenas* locals = caml_refresh_locals(stack);

    scan_stack_frames(f, fflags, fdata, stack, gc_regs, locals);

    f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));

    scan_local_allocations(f, fdata, locals, stack->local_sp);

    stack = Stack_parent(stack);
  }
}

void caml_maybe_expand_stack (void)
{
  struct stack_info* stk = Caml_state->current_stack;
  uintnat stack_available =
    (value*)stk->sp - Stack_base(stk);
  uintnat stack_needed =
    Stack_threshold / sizeof(value)
    /* for words pushed by caml_start_program */
    + 8 + Stack_padding_word;

  if (stack_available < stack_needed) {
    if (!caml_try_realloc_stack (stack_needed)) {
      caml_raise_stack_overflow();
    }
  }

  if (Caml_state->gc_regs_buckets == NULL) {
    /* Ensure there is at least one gc_regs bucket available before
       running any OCaml code. See fiber.h for documentation. */
    value* bucket = caml_stat_alloc(sizeof(value) * Wosize_gc_regs);
    bucket[0] = 0; /* no next bucket */
    Caml_state->gc_regs_buckets = bucket;
  }
}

#else /* End NATIVE_CODE, begin BYTE_CODE */

value caml_global_data;

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  value* sp;
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, id);

  if (!stack) caml_raise_out_of_memory();

  sp = Stack_high(stack);
  sp -= 1;
  sp[0] = Val_long(1);

  stack->sp = sp;

  return Val_ptr(stack);
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->current_stack->sp - req <
      Stack_base(Caml_state->current_stack))
    if (!caml_try_realloc_stack(req))
      caml_raise_stack_overflow();
  return Val_unit;
}

/*
  Root scanning.

  Used by the GC to find roots on the stacks of running or runnable fibers.
*/

/* Code pointers are stored on the bytecode stack as naked pointers.
   We must avoid passing them to the scanning action,
   unless we know that it is a no-op outside young values
   (so it will safely ignore code pointers). */
 Caml_inline int is_scannable(scanning_action_flags flags, value v) {
  return
      (flags & SCANNING_ONLY_YOUNG_VALUES)
      || (Is_block(v) && caml_find_code_fragment_by_pc((char *) v) == NULL);
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* v_gc_regs)
{
  value *low, *high, *sp;

  while (stack != NULL) {
    CAMLassert(stack->magic == 42);

    high = Stack_high(stack);
    low = stack->sp;
    for (sp = low; sp < high; sp++) {
      value v = *sp;
      if (is_scannable(fflags, v)) {
        f(fdata, v, sp);
      }
    }

    if (is_scannable(fflags, Stack_handle_value(stack)))
      f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    if (is_scannable(fflags, Stack_handle_exception(stack)))
      f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    if (is_scannable(fflags, Stack_handle_effect(stack)))
      f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));

    stack = Stack_parent(stack);
  }
}

#endif /* end BYTE_CODE */

CAMLexport void caml_do_local_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct caml__roots_block *local_roots,
  struct stack_info *current_stack,
  value * v_gc_regs)
{
  struct caml__roots_block *lr;
  int i, j;
  value* sp;
#ifdef NATIVE_CODE
  caml_local_arenas* locals = caml_refresh_locals(current_stack);
#endif

  for (lr = local_roots; lr != NULL; lr = lr->next) {
    for (i = 0; i < lr->ntables; i++){
      for (j = 0; j < lr->nitems; j++){
        sp = &(lr->tables[i][j]);
        if (*sp != 0) {
#ifdef NATIVE_CODE
          visit (f, fdata, locals, caml_global_heap_state, sp);
#else
          f (fdata, *sp, sp);
#endif
        }
      }
    }
  }
  caml_scan_stack(f, fflags, fdata, current_stack, v_gc_regs);
#ifndef NATIVE_CODE
  CAMLassert(current_stack->local_arenas == NULL);
#endif
}


/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

#ifdef NATIVE_CODE
/* Update absolute exception pointers for new stack*/
void caml_rewrite_exception_stack(struct stack_info *old_stack,
                                  value** exn_ptr, value** async_exn_ptr,
                                  struct stack_info *new_stack)
{
  fiber_debug_log("Old [%p, %p]", Stack_base(old_stack), Stack_high(old_stack));
  fiber_debug_log("New [%p, %p]", Stack_base(new_stack), Stack_high(new_stack));
  if(exn_ptr) {
    CAMLassert(async_exn_ptr != NULL);

    fiber_debug_log ("*exn_ptr=%p", *exn_ptr);
    fiber_debug_log ("*async_exn_ptr=%p", *async_exn_ptr);

    while (Stack_base(old_stack) < *exn_ptr &&
           *exn_ptr <= Stack_high(old_stack)) {
      int must_update_async_exn_ptr = *exn_ptr == *async_exn_ptr;
#ifdef DEBUG
      value* old_val = *exn_ptr;
#endif
      *exn_ptr = Stack_high(new_stack) - (Stack_high(old_stack) - *exn_ptr);

      if (must_update_async_exn_ptr) *async_exn_ptr = *exn_ptr;
      fiber_debug_log ("must_update_async_exn_ptr=%d",
        must_update_async_exn_ptr);

      fiber_debug_log ("Rewriting %p to %p", old_val, *exn_ptr);

      CAMLassert(Stack_base(new_stack) < *exn_ptr);
      CAMLassert((value*)*exn_ptr <= Stack_high(new_stack));

      exn_ptr = (value**)*exn_ptr;
    }
    fiber_debug_log ("finished with *exn_ptr=%p", *exn_ptr);
  } else {
    fiber_debug_log ("exn_ptr is null");
    CAMLassert(async_exn_ptr == NULL);
  }
}
#endif

int caml_try_realloc_stack(asize_t required_space)
{
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
  (void) required_space;
  return 0;
#else
  struct stack_info *old_stack, *new_stack;
  asize_t wsize;
  int stack_used;
  CAMLnoalloc;

  old_stack = Caml_state->current_stack;
  stack_used = Stack_high(old_stack) - (value*)old_stack->sp;
  wsize = Stack_high(old_stack) - Stack_base(old_stack);
  uintnat max_stack_wsize = caml_max_stack_wsize;
  do {
    if (wsize >= max_stack_wsize) return 0;
    wsize *= 2;
  } while (wsize < stack_used + required_space);

  if (wsize > 4096 / sizeof(value)) {
    CAML_GC_MESSAGE(STACKS,
                    "Growing stack to %"
                    ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                    Bsize_wsize(wsize) / 1024);
  } else {
    CAML_GC_MESSAGE(STACKS,
                    "Growing stack to %"
                    ARCH_INTNAT_PRINTF_FORMAT "u bytes\n",
                    Bsize_wsize(wsize) * sizeof(value));
  }

  new_stack = caml_alloc_stack_noexc(wsize,
                                     Stack_handle_value(old_stack),
                                     Stack_handle_exception(old_stack),
                                     Stack_handle_effect(old_stack),
                                     old_stack->id);

  if (!new_stack) return 0;
  memcpy(Stack_high(new_stack) - stack_used,
         Stack_high(old_stack) - stack_used,
         stack_used * sizeof(value));
  new_stack->sp = Stack_high(new_stack) - stack_used;
  Stack_parent(new_stack) = Stack_parent(old_stack);

  new_stack->local_arenas = caml_refresh_locals(old_stack);
  new_stack->local_sp = old_stack->local_sp;
  new_stack->local_top = old_stack->local_top;
  new_stack->local_limit = old_stack->local_limit;

  // Detach locals stack from old_stack so it will not be freed
  old_stack->local_arenas = NULL;
  old_stack->local_sp = 0;
  old_stack->local_top = NULL;
  old_stack->local_limit = 0;

#ifdef NATIVE_CODE
  /* There's no need to do another pass rewriting from
     Caml_state->async_exn_handler because every asynchronous exception trap
     frame is also a normal exception trap frame.  However
     Caml_state->async_exn_handler itself must be updated. */
  caml_rewrite_exception_stack(old_stack, (value**)&Caml_state->exn_handler,
                               (value**) &Caml_state->async_exn_handler,
                               new_stack);
#endif

  /* Update stack pointers in Caml_state->c_stack. It is possible to have
   * multiple c_stack_links to point to the same stack since callbacks are run
   * on existing stacks. */
  {
    struct c_stack_link* link;
    for (link = Caml_state->c_stack; link; link = link->prev) {
      if (link->stack == old_stack) {
        ptrdiff_t delta =
          (char*)Stack_high(new_stack) - (char*)Stack_high(old_stack);
#ifdef WITH_FRAME_POINTERS
        struct stack_frame {
          struct stack_frame* prev;
          void* retaddr;
        };

        /* Frame pointer is pushed just below the c_stack_link.
           This is somewhat tricky to guarantee when there are stack
           arguments to C calls: see caml_c_call_copy_stack_args */
        struct stack_frame* fp = ((struct stack_frame*)link) - 1;
        CAMLassert(fp->prev == link->sp);

        /* Rewrite OCaml frame pointers above this C frame */
        while (Stack_base(old_stack) <= (value*)fp->prev &&
               (value*)fp->prev < Stack_high(old_stack)) {
          fp->prev = (struct stack_frame*)((char*)fp->prev + delta);
          fp = fp->prev;
        }
#endif
        link->stack = new_stack;
        link->sp = (char*)link->sp + delta;
      }
      if (link->async_exn_handler >= (char*) Stack_base(old_stack)
          && link->async_exn_handler < (char*) Stack_high(old_stack)) {
        /* The asynchronous exception trap frame pointed to by the current
           c_stack_link lies on the OCaml stack being reallocated.  Repoint the
           trap frame to the new stack. */
        fiber_debug_log("Rewriting link->async_exn_handler %p...",
          link->async_exn_handler);
        link->async_exn_handler +=
          (char*) Stack_high(new_stack) - (char*) Stack_high(old_stack);
        fiber_debug_log("...to %p", link->async_exn_handler);
      } else {
        fiber_debug_log("Not touching link->async_exn_handler %p",
          link->async_exn_handler);
      }
    }
  }

  caml_free_stack(old_stack);
  Caml_state->current_stack = new_stack;
  return 1;
#endif
}

struct stack_info* caml_alloc_main_stack (uintnat init_wsize)
{
  const int64_t id = atomic_fetch_add(&fiber_id, 1);
  struct stack_info* stk =
    caml_alloc_stack_noexc(init_wsize, Val_unit, Val_unit, Val_unit, id);
  return stk;
}

static void free_stack_memory(struct stack_info* stack)
{
#if defined(DEBUG) && defined(STACK_CHECKS_ENABLED)
  memset(stack, 0x42, (char*)stack->handler - (char*)stack);
#endif
#if defined(USE_MMAP_MAP_STACK)
  munmap(stack, stack->size);
#elif defined(STACK_GUARD_PAGES)
  caml_mem_unmap(stack, stack->size);
#else
  caml_stat_free(stack);
#endif
}

void caml_free_stack (struct stack_info* stack)
{
  CAMLnoalloc;
  struct stack_info** cache = Caml_state->stack_cache;

  CAMLassert(stack->magic == 42);
  CAMLassert(cache != NULL);

  // Don't need to update local_sp since this is no longer the current stack.
  caml_free_local_arenas(stack->local_arenas);

  if (stack->cache_bucket != -1) {
    stack->exception_ptr =
      (void*)(cache[stack->cache_bucket]);
    cache[stack->cache_bucket] = stack;
#if defined(DEBUG) && defined(STACK_CHECKS_ENABLED)
    memset(Stack_base(stack), 0x42,
           (Stack_high(stack)-Stack_base(stack))*sizeof(value));
#endif
  } else {
    free_stack_memory(stack);
  }
}

void caml_free_gc_regs_buckets(value *gc_regs_buckets)
{
  while (gc_regs_buckets != NULL) {
    value *next = (value*)gc_regs_buckets[0];
    caml_stat_free(gc_regs_buckets);
    gc_regs_buckets = next;
  }
}


CAMLprim value caml_continuation_use_noexc (value cont)
{
  value v;
  value null_stk = Val_ptr(NULL);
  CAMLnoalloc;

  fiber_debug_log("cont: is_block(%d) tag_val(%ul) is_young(%d)",
                  Is_block(cont), Tag_val(cont), Is_young(cont));
  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);

  /* this forms a barrier between execution and any other domains
     that might be marking this continuation */
  if (!Is_young(cont) && caml_marking_started())
    caml_darken_cont(cont);

  v = Field(cont, 0);

  if (caml_domain_alone()) {
    Field(cont, 0) = null_stk;
    return v;
  }

  if (atomic_compare_exchange_strong(Op_atomic_val(cont), &v, null_stk)) {
    return v;
  } else {
    return null_stk;
  }
}

CAMLprim value caml_continuation_use (value cont)
{
  value v = caml_continuation_use_noexc(cont);
  if (v == Val_ptr(NULL))
    caml_raise_continuation_already_resumed();
  return v;
}

CAMLprim value caml_continuation_use_and_update_handler_noexc
  (value cont, value hval, value hexn, value heff)
{
  value stack;
  struct stack_info* stk;

  stack = caml_continuation_use_noexc (cont);
  stk = Ptr_val(stack);
  if (stk == NULL) {
    /* The continuation has already been taken */
    return stack;
  }
  while (Stack_parent(stk) != NULL) stk = Stack_parent(stk);
  Stack_handle_value(stk) = hval;
  Stack_handle_exception(stk) = hexn;
  Stack_handle_effect(stk) = heff;
  return stack;
}

void caml_continuation_replace(value cont, struct stack_info* stk)
{
  value n = Val_ptr(NULL);
  int b = atomic_compare_exchange_strong(Op_atomic_val(cont), &n, Val_ptr(stk));
  CAMLassert(b);
  (void)b; /* squash unused warning */
}

CAMLprim value caml_drop_continuation (value cont)
{
  struct stack_info* stk = Ptr_val(caml_continuation_use(cont));
  caml_free_stack(stk);
  return Val_unit;
}

static const value * _Atomic caml_unhandled_effect_exn = NULL;
static const value * _Atomic caml_continuation_already_resumed_exn = NULL;

static const value * cache_named_exception(const value * _Atomic * cache,
                                           const char * name)
{
  const value * exn;
  exn = atomic_load_acquire(cache);
  if (exn == NULL) {
    exn = caml_named_value(name);
    if (exn == NULL) {
      fprintf(stderr, "Fatal error: exception %s\n", name);
      exit(2);
    }
    atomic_store_release(cache, exn);
  }
  return exn;
}

CAMLexport void caml_raise_continuation_already_resumed(void)
{
  const value * exn =
    cache_named_exception(&caml_continuation_already_resumed_exn,
                          "Effect.Continuation_already_resumed");
  caml_raise(*exn);
}

value caml_make_unhandled_effect_exn (value effect)
{
  CAMLparam1(effect);
  value res;
  const value * exn =
    cache_named_exception(&caml_unhandled_effect_exn, "Effect.Unhandled");
  res = caml_alloc_small(2,0);
  Field(res, 0) = *exn;
  Field(res, 1) = effect;
  CAMLreturn(res);
}

CAMLexport void caml_raise_unhandled_effect (value effect)
{
  caml_raise(caml_make_unhandled_effect_exn(effect));
}
