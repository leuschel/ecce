/*
 * new_alloc.c -- New memory manager.
 * AFSID           : $__Header$
 * Author          : Manuel Carro
 * Created On      : Mon Dec 15 13:50:46 1997
 * Last Modified By: MCL
 * Last Modified On: Fri Dec 17 17:23:42 1999
 * Update Count    : 862
 * Status          : Seems to work correctly
 */

#include <stdlib.h>
#include <string.h>

#if !defined(Win32i86)
# include <strings.h>
#endif

#if defined(DEBUG)
#include <stdio.h>
#endif

#include "configure.h"
#include "termdefs.h"
#include "debug.h"
#include "own_malloc_defs.h"

#if defined(USE_OWN_MALLOC) && defined(USE_MMAP)
#include <sys/mman.h>
#endif

#if defined(USE_OWN_MALLOC)

#if defined(USE_MMAP)
static char *mmap_top = NULL;
static int mmap_size = 0;
#endif

#include <stdio.h>
void init_own_malloc() {
#if defined(USE_MMAP)
  char *d;
  char *mmap_start;
  mmap_size = 0x10000000;
  mmap_start = (char *)MallocBase;
  d = mmap(mmap_start, mmap_size, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE, 0, 0);
  fprintf(stderr, "mmap own memory at %p\n", d);
  if (d != mmap_start) {
    fprintf(stderr, "panic: cannot mmap own memory at %p\n", mmap_start);
    exit(-1);
  }
  mmap_top = mmap_start;
#endif
}

#if !defined(TRUE)                     /* Probably standalone compilation */
#define TRUE 1
#define FALSE 0
typedef unsigned long int TAGGED;                 /* base to align memory */
typedef int BOOL;
#endif

#if !defined(MIN_MEM_ALLOC)
#define MIN_MEM_ALLOC 32768                /* TAGGED --  from configure.h */
#endif

#define ALIGN sizeof(TAGGED)       /* blocks suitably aligned for any use */
#define TW_TO_CHARS(Tw) (Tw)*ALIGN
#define CHARS_TO_TW(Chars) ((Chars)%ALIGN==0 ? (Chars)/ALIGN : (Chars)/ALIGN+1)

static int min_mem_alloc = MIN_MEM_ALLOC;

/* List of free and asigned blocks. Kept in memory order, to allow
   recompaction upon block freeing.  Use forward and backwards link for
   this.  Compaction is not always possible: blocks of memory can come from
   sparse addresses. */

struct mem_block_str {
  BOOL block_is_free;                            /* Might be only one bit */
  struct mem_block_str *fwd, *bck;
  struct mem_block_str *next_free, *prev_free;
  int size;                                   /* Measured in TAGGED words */
  TAGGED mem_ptr;                     /* 1st word of the allocated memory */
};

typedef struct mem_block_str MEM_BLOCK;

#define HEADER_SIZE ((CHARS_TO_TW(sizeof(MEM_BLOCK))) - 1)

static MEM_BLOCK *block_list = NULL;
static MEM_BLOCK *free_block_list = NULL;

static MEM_BLOCK *locate_block(int tagged_w);
static TAGGED *reserve_block(int requested_chars, MEM_BLOCK *list);
static void insert_block(MEM_BLOCK *block, 
                         MEM_BLOCK *previous,
                         MEM_BLOCK *next);
static void insert_free_block(MEM_BLOCK *block);
static void remove_free_block(MEM_BLOCK *block);
static MEM_BLOCK *create_new_block(int size_in_chars);
static void dealloc_block(MEM_BLOCK *block);

#define find_mem_block(Ptr) ((MEM_BLOCK *)(ptr - HEADER_SIZE))

/* Search for a block with memory enough. Requested size comes in TAGGED
   words.  If no block found, return NULL. */

static MEM_BLOCK *locate_block(requested_tagged_w)
     int requested_tagged_w;
{
  MEM_BLOCK *running;

#if defined(DEBUG)
  if (debug_mem)
    printf("locate_block was requested a block of %d words\n",
           requested_tagged_w);
  if (requested_tagged_w < 1)
    printf("Uh? locate_block was requested a block of %d words!\n",
           requested_tagged_w);
#endif

    running = free_block_list;
    while(running && running->size < requested_tagged_w) 
      running = running->next_free;

#if defined(DEBUG)
  if (debug_mem) {
    if (!running )
      printf("locate_block did not find a block of %d words\n",
             requested_tagged_w);
  }
#endif
  
  return running;
}


/* Given a block which has enough room to allocate the requested chars,
   return pointer to memory area and split the block into two if needed. */

static TAGGED *reserve_block(req_tagged, block)
     int        req_tagged;
     MEM_BLOCK *block;
{
  MEM_BLOCK *new_block;

#if defined(DEBUG)
  if (block->size < req_tagged){
    printf("**** Fatal error: reserve_block received a block smaller than needed\n");
    return NULL;
  }
#endif

  if (block->size > req_tagged + HEADER_SIZE) {
    /* Block is still free -- do not remove from free blocks list  */
    block->size -= (req_tagged + HEADER_SIZE);
    new_block = (MEM_BLOCK *)((TAGGED *)block + HEADER_SIZE + block->size);
    new_block->block_is_free = FALSE;
    new_block->size = req_tagged;
    insert_block(new_block, block, block->fwd);
    return &(new_block->mem_ptr);
  } else {                                    /* Exactly the size we want */
    block->block_is_free = FALSE;
    remove_free_block(block);
    return &(block->mem_ptr);
  }
}


/* Link a block into the list.  previous == NULL if first block, next ==
   NULL if last block . */

static void insert_block(block, previous, next)
     MEM_BLOCK *block, *previous, *next;
{
  block->bck = previous;
  block->fwd = next;
  if (previous != NULL)
    previous->fwd = block;
  else                             /* Then block is the first in the list */
    block_list = block;
  if (next != NULL) next->bck = block;
}


/* Link a block into the free blocks list. */

static void insert_free_block(block)
     MEM_BLOCK *block;
{
  block->prev_free = NULL;
  block->next_free = free_block_list;
  if (free_block_list)
    free_block_list->prev_free = block;
  free_block_list = block;    
#if defined(DEBUG)
  if (debug_mem)
    printf("*Put back block of %d words\n", block->size);
#endif
}

/* Remove a block from the free blocks list. */

static void remove_free_block(block)
     MEM_BLOCK *block;
{
  MEM_BLOCK *previous = block->prev_free,
            *next     = block->next_free;

  if (next)
    next->prev_free = previous;
  if (previous)
    previous->next_free = next;
  else 
    free_block_list = next;
}


/* Create a new block with a given size, rounded upwards to be a multiple of
   an ALIGNed word.  If the creation was sucessful, insert them into the
   general blocks list and into the free blocks list. */

static MEM_BLOCK *create_new_block(int size_in_tagged_w) {
  MEM_BLOCK *new_block;

#if defined(USE_MMAP)
  char *new_mmap_top;
  new_mmap_top = mmap_top + TW_TO_CHARS(size_in_tagged_w + HEADER_SIZE);
  if (new_mmap_top - (char *)MallocBase > mmap_size) {
    new_block = NULL;
  } else {
    new_block = (MEM_BLOCK *)mmap_top;
    mmap_top = new_mmap_top;
  }
#else
  new_block = (MEM_BLOCK *)malloc(TW_TO_CHARS(size_in_tagged_w + HEADER_SIZE));
  
  if (!new_block) {
#if defined(DEBUG)
    printf("malloc: could not allocate %d words of memory\n", 
           size_in_tagged_w + HEADER_SIZE);
#endif
    return NULL;
  }
#endif
  
  new_block->size = size_in_tagged_w;
  new_block->block_is_free = TRUE;
  insert_block(new_block, NULL, block_list);
  insert_free_block(new_block);
  return new_block;
}

/* Mark a block as unused.  Collapse it with the surronding ones if possible */

static void dealloc_block(block)
     MEM_BLOCK *block;
{
  MEM_BLOCK *next, *prev;

  block->block_is_free = TRUE;

  if (((next = block->fwd) != NULL) &&        /* Check if next block free */
      (next->block_is_free == TRUE) &&
      ((TAGGED *)block + block->size + HEADER_SIZE == (TAGGED *)next)){
    block->size += next->size + HEADER_SIZE;
    if ((block->fwd = next->fwd) != NULL)
      block->fwd->bck = block;
    remove_free_block(next);
  }
  
  if (((prev = block->bck) != NULL) &&
      (prev->block_is_free == TRUE) &&
      ((TAGGED *)prev + prev->size + HEADER_SIZE == (TAGGED *)block)){
    prev->size += block->size + HEADER_SIZE;
    if ((prev->fwd = block->fwd) != NULL)
      prev->fwd->bck = prev;
  } else insert_free_block(block);
}


 /* Our three beloved calls: alloc, dealloc, realloc. */

#define ADJUST_BLOCK(size) (size < min_mem_alloc ? min_mem_alloc : size)

TAGGED *own_malloc(size)
     int size;
{
  MEM_BLOCK *block;
  TAGGED *pointer_returned;
  int size_to_reserve;

#if defined(DEBUG)
  if (size <= 0){
    printf("own_malloc received a request of %d chars... what should I do?\n",
           size);
    return NULL;
  }
#endif
  
  size_to_reserve = CHARS_TO_TW(size);
  if ((block = locate_block(size_to_reserve)) == NULL) {
    block = create_new_block(ADJUST_BLOCK(size_to_reserve));
    if (block == NULL){                     /* malloc could not stand it! */
#if defined(DEBUG)
      printf("own_malloc: could not reserve %d chars\n", size);
#endif
      return NULL;
    }
    /* min_mem_alloc *= 2; */
  }
  pointer_returned = reserve_block(size_to_reserve, block);
#if defined(DEBUG)
  if (debug_mem)
    printf("own_malloc returned %x, %d chars\n", 
           (unsigned int)pointer_returned, size);
#endif

  return pointer_returned;
}

void own_free(ptr)
     TAGGED *ptr;
{
  MEM_BLOCK *block_to_dealloc;

  if ((block_to_dealloc = find_mem_block(ptr))){
    dealloc_block(block_to_dealloc);
  }
}


/* From the manpages:
   realloc() changes the size of the block pointed to by ptr to size bytes
   and returns a pointer to the (possibly moved) block.  The contents will
   be unchanged up to the lesser of the new and old sizes.  If ptr is NULL,
   realloc() behaves like malloc() for the specified size.  If size is zero
   and ptr is not a null pointer, the object pointed to is freed.
*/

TAGGED *own_realloc(ptr,size)                            /* size in chars */
     TAGGED *ptr;
     int     size;
{
  MEM_BLOCK *old_block;
  int        size_to_copy;

  if (ptr == NULL) 
    return own_malloc(size);
  else if (size == 0) {
    own_free(ptr);
    return NULL;
  }

  old_block = find_mem_block(ptr);                /* Gives error messages */
  if (old_block) {
    MEM_BLOCK *new_block;
    TAGGED    *new_mem_area;
    int        size_in_tagged_w = CHARS_TO_TW(size);

    if ((new_block = locate_block(size_in_tagged_w)) == NULL) {
      new_block = create_new_block(ADJUST_BLOCK(size_in_tagged_w));
      if (new_block == NULL){
#if defined(DEBUG)
        printf("own_realloc: could not reserve %d chars!\n", size);
#endif
        return NULL;
      } 
      /* min_mem_alloc *= 2; */
    }
    
    new_mem_area = reserve_block(size_in_tagged_w, new_block);
    size_to_copy = old_block->size > size_in_tagged_w ?
                   size_in_tagged_w : old_block->size;
    (void)memcpy(new_mem_area, 
                 &(old_block->mem_ptr),
                 TW_TO_CHARS(size_to_copy));
    own_free(ptr);
#if defined(DEBUG)
    if (debug_mem)
      printf("own_realloc returned %x, %d chars\n",
             (unsigned int)new_mem_area, size);
#endif    

    return new_mem_area;
  } else return NULL;
} 

#endif                                                  /* USE_OWN_MALLOC */
