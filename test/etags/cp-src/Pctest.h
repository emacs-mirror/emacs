// -*- c++ -*-
//
// $Id: Pctest.h,v 1.14 2000/01/19 17:14:42 bmah Exp $
//
// Pctest.h
// Bruce A. Mah <bmah@ca.sandia.gov>
//
// This work was first produced by an employee of Sandia National
// Laboratories under a contract with the U.S. Department of Energy.
// Sandia National Laboratories dedicates whatever right, title or
// interest it may have in this software to the public. Although no
// license from Sandia is needed to copy and use this software,
// copying and using the software might infringe the rights of
// others. This software is provided as-is. SANDIA DISCLAIMS ANY
// WARRANTY OF ANY KIND, EXPRESS OR IMPLIED.
//
// Header for virtual base class of tests.  A particular protocol (e.g.
// IPv4, IPv6) will override the methods of this base class
// with protocol-specific implementations.
//
//

#ifndef PCTEST_H
#define PCTEST_H

#include <stdio.h>

#if STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#endif /* STDC_HEADERS */

#if HAVE_UNISTD_H
#include <sys/types.h>
#endif /* HAVE_UNISTD_H */

#include <sys/socket.h>
#include <sys/time.h>

#include "pc.h"
// #include "TestRecord.h"
class TestRecord;

// Action codes.  ICMPv4 and ICMPv6 have different values for their type
// and code fields.  The Pctest abstracts these differences.
typedef enum {
    PctestActionValid,		// store valid measurement (e.g. ICMP
				// time exceeded)
    PctestActionValidLasthop,	// store valid measurement, this is last hop
				// (e.g. ICMP port unreachable)
    PctestActionFiltered,	// packets filtered, give up (e.g.
				// ICMP prohibited)
    PctestActionAbort		// huh?  we haven't a clue
} PctestActionType;

class Pctest {

  public:
    Pctest() {
	initialized = 0;
	TimeSyscall(syscallTime);

	IF_DEBUG(3, fprintf(stderr, "syscallTime.tv_usec = %ld\n", syscallTime.tv_usec));
    }
    virtual ~Pctest() { };

    // Get gettimeofday() system call overhead.
    virtual void TimeSyscall(struct timeval &diff);

    // Get random payload buffer
    virtual char *GeneratePayload(int size);

    // Set target host for our tests (resolve if necessary)
    virtual int SetTargetName(char *target) = 0;

    // Get target host name and address
    virtual char *GetTargetName() { return targetName; };
    virtual char *GetPrintableAddress() = 0;
    virtual char *GetPrintableAddress(void *a) = 0;
    virtual char *GetName(void *a) = 0;
    virtual char *GetAddressFamily() = 0;

    // Perform a test and return statistics
    virtual int Test(TestRecord &tr) = 0;
    virtual PctestActionType GetAction(TestRecord &tr) = 0;
    virtual PctestActionType GetAction(TestRecord *tr) {
	return this->GetAction(*tr);
    };

    virtual int GetMinSize() = 0;

  protected:
    int initialized;		// initialization flag
    char *targetName;		// target hostname
    struct timeval syscallTime;	// estimated overhead for gettimeofday()

};

#endif /* PCTEST_H */
