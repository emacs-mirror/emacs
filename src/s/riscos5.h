#include "bsd4-3.h"

/* This file has changes that Jost Krieger <x920031@rubb.rz.ruhr-uni-bochum.de>
   says are necessary.  */

/* No declaration in system header files.  */
extern double atof ();

#define LD_SWITCH_SYSTEM -non_shared

#define GETPGRP_NO_ARG
