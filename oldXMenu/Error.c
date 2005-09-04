#include "copyright.h"

/* Copyright    Massachusetts Institute of Technology    1985	*/
/* Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.  */

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuError -	Returns a string description of the current
 *			XMenu error status flag.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

char *
XMenuError()
{
    static char message[128];		/* Error message buffer. */

    if ((_XMErrorCode < XME_CODE_COUNT) && (_XMErrorCode >= 0)) {
	return(_XMErrorList[_XMErrorCode]);
    }
    sprintf(message, "Unknown _XMErrorCode: %d", _XMErrorCode);
    return(message);
}

/* arch-tag: 5fff4a23-40ca-40d0-8887-c50fc73dea9d
   (do not change this comment) */
