#ifndef vmstime_h
#define vmstime_h

#include <time.h>
#include <libdtdef.h>

extern long timezone;
extern int daylight;
extern char *tzname[2];

void sys_tzset();
struct tm *sys_localtime(time_t *clock);
struct tm *sys_gmtime(time_t *clock);

#endif /* vmstime_h */
