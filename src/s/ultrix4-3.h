#include "bsd4-3.h"

#define NEED_UNISTD_H
#define HAVE_TERMIOS
#define PREFER_VSUSP

/* Must set the line discipline to this, to make termio work.  */
#define SET_LINE_DISCIPLINE TERMIODISC
