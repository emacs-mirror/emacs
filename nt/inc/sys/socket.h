/* Copyright (C) 1995, 2001-2017 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */


/* Workable version of <sys/socket.h> based on winsock.h */

#ifndef _SOCKET_H_
#define _SOCKET_H_

/* defeat the multiple include protection */
#ifdef _WINSOCKAPI_
#undef _WINSOCKAPI_
#endif
#ifdef _WINSOCK_H
#undef _WINSOCK_H
#endif

/* avoid confusion with our version of select */
#ifdef select
#undef select
#define MUST_REDEF_SELECT
#endif

/* avoid clashing with our version of FD_SET if already defined */
#ifdef FD_SET
#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO
#endif

/* Avoid duplicate definition of timeval.  MinGW uses _TIMEVAL_DEFINED
   in sys/time.h to avoid that.  */
#if defined (HAVE_TIMEVAL) && defined (_MSC_VER)
#define timeval ws_timeval
#endif

#include <winsock2.h>
#include <ws2tcpip.h>
/* process.c uses uint16_t (from C99) for IPv6, but
   apparently it is not defined in some versions of mingw and msvc.  */
#include <stdint.h>
#ifndef UINT16_C
typedef unsigned short uint16_t;
#endif

/* redefine select to reference our version */
#ifdef MUST_REDEF_SELECT
#define select sys_select
#undef MUST_REDEF_SELECT
#endif

/* Revert to our version of FD_SET, but not when included from test
   programs run by configure.  */
#ifdef EMACS_CONFIG_H
#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO

/* allow us to provide our own version of fd_set */
#define fd_set ws_fd_set
#include "w32.h"
#endif	/* EMACS_CONFIG_H */

#if defined (HAVE_TIMEVAL) && defined (_MSC_VER)
#undef timeval
#endif

/* shadow functions where we provide our own wrapper */
#define socket         sys_socket
#define bind           sys_bind
#define connect        sys_connect
#define htons          sys_htons
#define ntohs          sys_ntohs
#define inet_addr      sys_inet_addr
#define gethostname    sys_gethostname
#define gethostbyname  sys_gethostbyname
#define getpeername    sys_getpeername
#define getservbyname  sys_getservbyname
#define shutdown       sys_shutdown
#define setsockopt     sys_setsockopt
#define listen         sys_listen
#define getsockname    sys_getsockname
#define accept         sys_accept
#define recvfrom       sys_recvfrom
#define sendto         sys_sendto
#define getaddrinfo    sys_getaddrinfo
#define freeaddrinfo   sys_freeaddrinfo

int sys_socket(int af, int type, int protocol);
int sys_bind (int s, const struct sockaddr *addr, int namelen);
int sys_connect (int s, const struct sockaddr *addr, int namelen);
u_short sys_htons (u_short hostshort);
u_short sys_ntohs (u_short netshort);
unsigned long sys_inet_addr (const char * cp);
int sys_gethostname (char * name, int namelen);
struct hostent * sys_gethostbyname (const char * name);
struct servent * sys_getservbyname (const char * name, const char * proto);
int sys_getpeername (int s, struct sockaddr *addr, int * namelen);
int sys_shutdown (int socket, int how);
int sys_setsockopt (int s, int level, int oname, const void * oval, int olen);
int sys_listen (int s, int backlog);
int sys_getsockname (int s, struct sockaddr * name, int * namelen);
int sys_accept (int s, struct sockaddr *addr, int *addrlen);
int sys_recvfrom (int s, char *buf, int len, int flags,
		  struct sockaddr *from, int * fromlen);
int sys_sendto (int s, const char * buf, int len, int flags,
		const struct sockaddr *to, int tolen);
int sys_getaddrinfo (const char * node, const char * service,
		     const struct addrinfo * hints, struct addrinfo ** res);
void sys_freeaddrinfo (struct addrinfo * ai);

/* In addition to wrappers for the winsock functions, we also provide
   an fcntl function, for setting sockets to non-blocking mode.  */
int fcntl (int s, int cmd, int options);
#define F_SETFL   4
#define F_SETFD   2
#define O_NONBLOCK  04000
#define O_CLOEXEC O_NOINHERIT
#define F_DUPFD_CLOEXEC 0x40000000
#define FD_CLOEXEC 1

/* we are providing a real h_errno variable */
#undef h_errno
extern int h_errno;

/* map winsock error codes to standard names */
#if defined(EWOULDBLOCK)
#undef EWOULDBLOCK
#endif
#define EWOULDBLOCK             WSAEWOULDBLOCK
#if defined(EINPROGRESS)
#undef EINPROGRESS
#endif
#define EINPROGRESS             WSAEINPROGRESS
#if defined(EALREADY)
#undef EALREADY
#endif
#define EALREADY                WSAEALREADY
#if defined(ENOTSOCK)
#undef ENOTSOCK
#endif
#define ENOTSOCK                WSAENOTSOCK
#if defined(EDESTADDRREQ)
#undef EDESTADDRREQ
#endif
#define EDESTADDRREQ            WSAEDESTADDRREQ
#if defined(EMSGSIZE)
#undef EMSGSIZE
#endif
#define EMSGSIZE                WSAEMSGSIZE
#if defined(EPROTOTYPE)
#undef EPROTOTYPE
#endif
#define EPROTOTYPE              WSAEPROTOTYPE
#if defined(ENOPROTOOPT)
#undef ENOPROTOOPT
#endif
#define ENOPROTOOPT             WSAENOPROTOOPT
#if defined(EPROTONOSUPPORT)
#undef EPROTONOSUPPORT
#endif
#define EPROTONOSUPPORT         WSAEPROTONOSUPPORT
#if defined(ESOCKTNOSUPPORT)
#undef ESOCKTNOSUPPORT
#endif
#define ESOCKTNOSUPPORT         WSAESOCKTNOSUPPORT
#if defined(EOPNOTSUPP)
#undef EOPNOTSUPP
#endif
#define EOPNOTSUPP              WSAEOPNOTSUPP
#if defined(EPFNOSUPPORT)
#undef EPFNOSUPPORT
#endif
#define EPFNOSUPPORT            WSAEPFNOSUPPORT
#if defined(EAFNOSUPPORT)
#undef EAFNOSUPPORT
#endif
#define EAFNOSUPPORT            WSAEAFNOSUPPORT
#if defined(EADDRINUSE)
#undef EADDRINUSE
#endif
#define EADDRINUSE              WSAEADDRINUSE
#if defined(EADDRNOTAVAIL)
#undef EADDRNOTAVAIL
#endif
#define EADDRNOTAVAIL           WSAEADDRNOTAVAIL
#if defined(ENETDOWN)
#undef ENETDOWN
#endif
#define ENETDOWN                WSAENETDOWN
#if defined(ENETUNREACH)
#undef ENETUNREACH
#endif
#define ENETUNREACH             WSAENETUNREACH
#if defined(ENETRESET)
#undef ENETRESET
#endif
#define ENETRESET               WSAENETRESET
#if defined(ECONNABORTED)
#undef ECONNABORTED
#endif
#define ECONNABORTED            WSAECONNABORTED
#if defined(ECONNRESET)
#undef ECONNRESET
#endif
#define ECONNRESET              WSAECONNRESET
#if defined(ENOBUFS)
#undef ENOBUFS
#endif
#define ENOBUFS                 WSAENOBUFS
#if defined(EISCONN)
#undef EISCONN
#endif
#define EISCONN                 WSAEISCONN
#if defined(ENOTCONN)
#undef ENOTCONN
#endif
#define ENOTCONN                WSAENOTCONN
#if defined(ESHUTDOWN)
#undef ESHUTDOWN
#endif
#define ESHUTDOWN               WSAESHUTDOWN
#if defined(ETOOMANYREFS)
#undef ETOOMANYREFS
#endif
#define ETOOMANYREFS            WSAETOOMANYREFS
#if defined(ETIMEDOUT)
#undef ETIMEDOUT
#endif
#define ETIMEDOUT               WSAETIMEDOUT
#if defined(ECONNREFUSED)
#undef ECONNREFUSED
#endif
#define ECONNREFUSED            WSAECONNREFUSED
#if defined(ELOOP)
#undef ELOOP
#endif
#define ELOOP                   WSAELOOP
/* #define ENAMETOOLONG            WSAENAMETOOLONG */
#if defined(EHOSTDOWN)
#undef EHOSTDOWN
#endif
#define EHOSTDOWN               WSAEHOSTDOWN
#if defined(EHOSTUNREACH)
#undef EHOSTUNREACH
#endif
#define EHOSTUNREACH            WSAEHOSTUNREACH
/* #define ENOTEMPTY               WSAENOTEMPTY */
#if defined(EPROCLIM)
#undef EPROCLIM
#endif
#define EPROCLIM                WSAEPROCLIM
#if defined(EUSERS)
#undef EUSERS
#endif
#define EUSERS                  WSAEUSERS
#if defined(EDQUOT)
#undef EDQUOT
#endif
#define EDQUOT                  WSAEDQUOT
#if defined(ESTALE)
#undef ESTALE
#endif
#define ESTALE                  WSAESTALE
#if defined(EREMOTE)
#undef EREMOTE
#endif
#define EREMOTE                 WSAEREMOTE

#endif /* _SOCKET_H_ */

/* end of socket.h */
