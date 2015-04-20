
/*
  static LOCK_BLOCK_P new_lock_block(LOCK_BLOCK_P old_block);
 */



/*
void init_dynamic_locks(void);
LOCK create_dynamic_lock(void);
*/

#if defined(DEBUG)

#if defined(Win32)
BOOL lock_is_unset_win32(LOCK *p);
#else
BOOL lock_is_unset(LOCK *p);
#endif

unsigned long int get_inc_counter(void);
void reset_counter(void);
#endif
