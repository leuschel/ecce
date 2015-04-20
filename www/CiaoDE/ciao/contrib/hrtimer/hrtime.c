/*
 * hrtime.c
 *
 * Copyright (C) 2000, Nick Rasmussen <nick@jive.org>
 *
 */
#include "hrtime.h"

#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>


static struct hrtime_struct *current = 0;

#define HRTIME_PATH_LENGTH 256

int
hrtime_is_present(void)
{
    struct stat tmp;

    if (stat("/proc/self/hrtime", &tmp) == 0)
        return 1;

    return 0;
}

int
hrtime_init(void)
{
    if (current != 0) return 0;
    return get_hrtime_struct(0, &current);
}

#if 0
int
hrtime_uninit(void)
{
    int error;
    if (current == 0) return 0;
    error = free_hrtime_struct(current);
    current = 0;
    return error;
}
#endif

int
get_hrtime_struct(pid_t pid, struct hrtime_struct **dest)
{
    char path[HRTIME_PATH_LENGTH];
    int  fd, count;

    if (dest == 0)
	return -1;

    if (pid == 0) pid = getpid();

    count = snprintf(path, HRTIME_PATH_LENGTH, "/proc/%d/hrtime", pid);
    if (count < 0 || count >= HRTIME_PATH_LENGTH)
        return -1;
	
    fd = open(path, O_RDONLY);
    if (fd < 0)
        return fd;
	
    *dest = (struct hrtime_struct *) mmap(0, sizeof(struct hrtime_struct),
					      PROT_READ, MAP_SHARED, fd, 0);
    close(fd);
	
    if (*dest == MAP_FAILED)
    {
        *dest = 0;
        return errno;
    }

    return 0;
} 

int
free_hrtime_struct(struct hrtime_struct *hr)
{
    int error;

    if (hr == 0) return 0;
    
    error = munmap((void *) hr, sizeof(struct hrtime_struct));
    if (error != 0)
        return error;

    return 0;
}

#define HRTIME_CYCLE_LIMIT 100

void
get_hrtime_self(hrtime_t *dest)
{
#if HRTIME_HAS_OFFSETS
    htrime_t tmp;
    long     offset;

    do {
	tmp = current->last_dispatch
	get_current_hrtime(dest);
	offset = current->offset_to_cpu0;
    }
    while (tmp != current->last_dispatch);

    /* start_time + offset is start_time relative to the cpu we are currently
     * executing on
     */
    *dest -= (current->start_time + offset);
#else
    get_current_hrtime(dest);
    *dest -= current->start_time;
#endif
}


void
get_hrtime(struct hrtime_struct *hr, hrtime_t *dest)
{
#if HRTIME_HAS_OFFSETS
    htrime_t tmp;
    long     offset;

    do {
	tmp = hr->last_dispatch
	get_current_hrtime(dest);
	offset = current->offset_to_cpu0;
    }
    while (tmp != hr->last_dispatch);

    /* start_time + offset is start_time relative to the cpu we are currently
     * executing on
     */
    *dest -= (hr->start_time + offset);
#else
    get_current_hrtime(dest);
    *dest -= hr->start_time;
#endif
}

void
get_hrvtime_self(hrtime_t *dest)
{
    hrtime_t ld, vtime;

    do {
	ld        = current->last_dispatch;
	vtime     = current->vtime;
	get_current_hrtime(dest);
    }
    while (ld != current->last_dispatch);

    *dest -= ld;
    *dest += vtime;
}

void
get_hrvtime(struct hrtime_struct *hr, hrtime_t *dest)
{
    hrtime_t ld, my_ld, vtime, tmp;

#if HRTIME_HAS_OFFSETS
    long     my_off, hr_off;
#endif

    do {
	ld        = hr->last_dispatch;
	my_ld     = current->last_dispatch;
	vtime     = hr->vtime;
#if HRTIME_HAS_OFFSETS
	my_off    = current->offset_to_cpu0;
	hr_off    = hr->offset_to_cpu0;
#endif
	get_current_hrtime(dest);
    }
    while (ld != hr->last_dispatch || my_ld != current->last_dispatch);

    if (ld != 0)
    {
#if HRTIME_HAS_OFFSETS
	/* make ld in reference to our cpu */
	ld += (hr_off - my_off);
#endif
	*dest -= ld;
	*dest += vtime;
    }
    else
    {
        /* queried process is not on a cpu, vtime is correct */
	*dest = vtime;
    }
}

void
get_hrutime_self(hrtime_t *dest)
{
    hrtime_t lud, utime;

    do {
	lud       = current->last_us_dispatch;
	utime     = current->utime;
	get_current_hrtime(dest);
    }
    while (lud != current->last_us_dispatch);

    *dest -= lud;
    *dest += utime;
}

void
get_hrutime(struct hrtime_struct *hr, hrtime_t *dest)
{
    hrtime_t lud, my_ld, utime, tmp;
    long     in_system;

#if HRTIME_HAS_OFFSETS
    long     my_off, hr_off;
#endif

    do {
	lud       = hr->last_us_dispatch;
	in_system = hr->in_system;
	my_ld     = current->last_dispatch;
	utime     = hr->utime;
#if HRTIME_HAS_OFFSETS
	my_off    = current->offset_to_cpu0;
	hr_off    = hr->offset_to_cpu0;
#endif
	get_current_hrtime(dest);
    }
    while (lud != hr->last_us_dispatch || my_ld != current->last_dispatch);

    if (lud != 0 && in_system == 0)
    {
#if HRTIME_HAS_OFFSETS
	/* make ld in reference to our cpu */
	lud += (hr_off - my_off);
#endif
	*dest -= lud;
	*dest += utime;
    }
    else
    {
	*dest = utime;
    }
}

void
get_hrstime_self(hrtime_t *dest)
{
    /* we know we are not in the system, stime is accurate */
    /* do we have a race still? can we be context switched out
     * halfway through a 64bit read? */
    *dest = current->stime;
}

void
get_hrstime(struct hrtime_struct *hr, hrtime_t *dest)
{
    hrtime_t lud, my_ld, stime, tmp;
    long     in_system;

#if HRTIME_HAS_OFFSETS
    long     my_off, hr_off;
#endif

    do {
	lud       = hr->last_us_dispatch;
	my_ld     = current->last_dispatch;
	stime     = hr->stime;
#if HRTIME_HAS_OFFSETS
	my_off    = current->offset_to_cpu0;
	hr_off    = hr->offset_to_cpu0;
#endif
	get_current_hrtime(dest);
    }
    while (lud != hr->last_us_dispatch || my_ld != current->last_dispatch);

    if (lud != 0 && in_system == 1)
    {
#if HRTIME_HAS_OFFSETS
	/* make ld in reference to our cpu */
	lud += (hr_off - my_off);
#endif
	*dest -= lud;
	*dest += stime;
    }
    else
    {
	*dest = stime;
    }
}
