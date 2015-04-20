/*
 * hrtime.h
 *
 * Copyright (C) 2000, Nick Rasmussen <nick@jive.org>
 *
 */
#ifndef _HRTIME_H_
#define _HRTIME_H_

#include "hrtime_shadow.h"
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Returns non-zero if the kernel was compiled with hrtime support
 */
int  hrtime_is_present(void);

/*
 * This must be called before any hrtime function is used
 */
int  hrtime_init(void);

/*
 * Map the hrtime_struct for the given process into the current process' address
 * space.
 *
 * pid is the pid of the process whose struct you wish to map, 0 indicating the
 * current process.
 * dest is changed to point to the desired process' hrtime_struct.
 *
 * If an error occurs, dest is set to null and a non-zero value is returned.
 */
int  get_hrtime_struct(pid_t pid, struct hrtime_struct **dest); 

/*
 * Unmap the given hrtime_struct from the current process' address space.
 * If an error occurs during unmapping, a non-zero value is returned.
 */
int  free_hrtime_struct(struct hrtime_struct *hr);


/*
 * These routines return the current high-resolution process times for the
 * given process.  The times returned are accurate with respect to the
 * time of the call of this function.  For performance reasons, no error
 * checking is done on the arguments.
 *
 * hr must be a struct returned from * get_hrtime_struct
 * dest must point to a valid hrtime_t
 */

/* Get the wall time for the current process */
void get_hrtime_self(hrtime_t *dest);

/* Get the wall time for the process associated with the hrtime_struct */
void get_hrtime(struct hrtime_struct *hr, hrtime_t *dest);

/* Get the virtual time for the current process */
void get_hrvtime_self(hrtime_t *dest);

/* Get the virtual time for the process associated with the hrtime_struct */
void get_hrvtime(struct hrtime_struct *hr, hrtime_t *dest);


/*
 * The following functions only produce valid results if the kernel
 * was compiled with CONFIG_HRUSTIME, the high-resolution user and system
 * time option.
 */

/* Get the user virtual time for the current process */
void get_hrutime_self(hrtime_t *dest);

/* Get the user virtual time for the process associated with the hrtime_struct */
void get_hrutime(struct hrtime_struct *hr, hrtime_t *dest);

/* Get the system virtual time for the current process */
void get_hrstime_self(hrtime_t *dest);

/* Get the system virtual time for the process associated with the hrtime_struct */
void get_hrstime(struct hrtime_struct *hr, hrtime_t *dest);

#ifdef __cplusplus
}
#endif

#endif /* _HRTIME_H_ */
